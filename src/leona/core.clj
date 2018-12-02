(ns leona.core
  (:refer-clojure :exclude [compile])
  (:require [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [com.walmartlabs.lacinia :as lacinia]
            [com.walmartlabs.lacinia.resolve :as lacinia-resolve]
            [com.walmartlabs.lacinia.schema :as lacinia-schema]
            [com.walmartlabs.lacinia.util :as lacinia-util]
            [leona.schema :as leona-schema]
            [leona.util :as util]))

(s/def ::query-spec keyword?)
(s/def ::mutation-spec keyword?)
(s/def ::resolver fn?)
(s/def ::query (s/keys :req-un [::resolver
                                ::query-spec]))
(s/def ::mutation (s/keys :req-un [::resolver
                                   ::mutation-spec]))
(s/def ::field-resolver (s/keys :req-un [::resolver]))
(s/def ::middleware (s/coll-of fn? :kind set))
(s/def ::specs (s/coll-of keyword? :kind set))
(s/def ::queries (s/map-of keyword? ::query))
(s/def ::mutations (s/map-of keyword? ::mutation))
(s/def ::field-resolvers (s/map-of keyword? ::field-resolver))
(s/def ::pre-compiled-data (s/keys :req-un [::specs
                                            ::queries
                                            ::mutations
                                            ::field-resolvers
                                            ::middleware]))
(s/def ::compiled map?)
(s/def ::compiled-data (s/keys :req-un [::compiled
                                        ::middleware]))

(defn build-middleware
  "Builds a fn which nests all the middleware and eventually the resolver"
  [resolver middleware args]
  (reduce (fn [a f] (apply partial (concat [f a] args))) resolver (reverse middleware)))

(defn error
  "Creates an error result as recognised by Lacinia"
  [{key :key message :message :as error-map}]
  (let [error-map (merge {:message (or message (name key))} error-map)]
    (lacinia-resolve/resolve-as nil error-map)))

(defn format-result
  "Format the results of a resolver"
  [m]
  (cske/transform-keys util/clj-name->qualified-gql-name m))

(defn format-input
  "Format the input into a resolver"
  [m]
  (cske/transform-keys util/gql-name->clj-name m))

(defn wrap-resolver
  "Used to wrap resolver fns provided by the user. This adds re-formatting in both directions and spec validation"
  [id resolver-fn input-spec result-spec]
  (fn [ctx input value]
    (let [formatted-input (format-input input)]
      (if-not (s/valid? input-spec formatted-input)
        (error {:key (keyword (str "invalid-" (name id)))
                :args (s/explain-data input-spec formatted-input)
                :message (str "The " (name id) " input didn't conform to the internal spec: " input-spec)})
        (let [resolver (-> (partial resolver-fn ctx formatted-input value)
                           (build-middleware (:middleware ctx) [ctx formatted-input value]))
              result (resolver)]
          (cond
            (instance? com.walmartlabs.lacinia.resolve.ResolverResultImpl result) result
            (s/valid? result-spec result) (format-result result)
            :else (error {:key (keyword (str "invalid-" (name id) "-result"))
                          :args (s/explain-data result-spec result)
                          :message (str "The " (name id) " result didn't conform to the internal spec: " result-spec)})))))))

(defn create
  "Creates an empty pre-compiled data structure for Leona"
  []
  {:specs #{}
   :queries {}
   :mutations {}
   :field-resolvers {}
   :middleware []})

(defn attach-field-resolver
  "Adds a field resolver into the provided pre-compiled data structure"
  [m field-spec resolver]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (-> m
      (update :specs   conj field-spec)
      (update :field-resolvers assoc field-spec {:resolver resolver})))

(defn attach-field-resolvers
  "Adds a series of field resolvers into the provided pre-compiled data structure"
  [m & field-pairs]
  {:pre [(s/valid? ::pre-compiled-data m)
         (not (zero? (count field-pairs)))
         (even? (count field-pairs))]}
  (reduce (fn [a [fs r]] (attach-field-resolver a fs r)) m (partition 2 field-pairs)))

(defn attach-middleware
  "Adds a middleware fn into the provided pre-compiled data structure"
  [m middleware-fn]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (update m :middleware conj middleware-fn))

(defn attach-query
  "Adds a query resolver into the provided pre-compiled data structure"
  ([m resolver]
   ;; TODO infer specs from fdef
   )
  ([m query-spec results-spec resolver]
   {:pre [(s/valid? ::pre-compiled-data m)]}
   (-> m
       (update :specs   conj results-spec)
       (update :queries assoc results-spec {:resolver resolver
                                            :query-spec query-spec}))))

(defn attach-mutation
  "Adds a mutation resolver fn into the provided pre-compiled data structure"
  ([m resolver]
   ;; TODO infer specs from fdef
   )
  ([m mutation-spec results-spec resolver]
   {:pre [(s/valid? ::pre-compiled-data m)]}
   (-> m
       (update :specs   conj results-spec)
       (update :mutations assoc results-spec {:resolver resolver
                                              :mutation-spec mutation-spec}))))

(defn- generate-root-objects
  "Generates root objects (mutations and queries) from the pre-compiled data structure"
  [m access-key id]
  (->> m
       (map (fn [[k v]]
              (hash-map (util/clj-name->gql-name k)
                        {:type (util/clj-name->gql-name k)
                         :args (get-in (leona-schema/transform (get v access-key))
                                       [:objects (util/clj-name->gql-name (get v access-key)) :fields])
                         :resolve (wrap-resolver id (:resolver v) (get v access-key) k)})))
       (apply merge)))

(defn- inject-field-resolver
  "Finds a field resolver from the provided collection and injects it into the appropriate place (object field)"
  [m field frs]
  (if-let [fr (some (fn [[k v]] (when (= (util/clj-name->gql-name k) field)
                                  (assoc v :spec k))) frs)]
    (assoc-in m [field :resolve] (wrap-resolver :field (:resolver fr) any? (:spec fr)))
    m))

(s/def ::field-with-type
  (s/map-of keyword? (s/and map? #(contains? % :type))))

(defn inject-field-resolvers
  "Walks a set of objects, attempting to inject field resolvers into certain types"
  [m frs]
  (update
   m :objects
   #(walk/postwalk
     (fn [d] (if (s/valid? ::field-with-type d)
               (inject-field-resolver d (-> d keys first) frs)
               d)) %)))

(defn generate
  "Takes pre-compiled data structure and converts it into a Lacinia schema"
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (cond-> (apply leona-schema/combine (:specs m))
    (not-empty (:queries m))         (assoc :queries   (generate-root-objects (:queries m)   :query-spec    :query))
    (not-empty (:mutations m))       (assoc :mutations (generate-root-objects (:mutations m) :mutation-spec :mutation))
    (not-empty (:field-resolvers m)) (inject-field-resolvers (:field-resolvers m))))

(defn compile
  "Generates a Lacinia schema from pre-compiled data structure and compiles it."
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (let [generated (generate m)]
    {:compiled   (lacinia-schema/compile generated)
     :generated  generated
     :middleware (:middleware m)}))

(defn execute
  "Executes Lacinia commands; adds middleware into the context which is required by the resolver wrapper"
  [m query]
  {:pre [(s/valid? ::compiled-data m)]}
  (lacinia/execute (:compiled m) query nil (select-keys m [:middleware])))
