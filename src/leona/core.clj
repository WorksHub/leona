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
   :middleware []
   :schemas []})

(defn attach-field-resolver
  "Adds a field resolver into the provided pre-compiled data structure"
  [m field-spec resolver]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (update m :field-resolvers assoc field-spec {:resolver resolver}))

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
  #_([m resolver]
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
  #_([m resolver]
     ;; TODO infer specs from fdef
     )
  ([m mutation-spec results-spec resolver]
   {:pre [(s/valid? ::pre-compiled-data m)]}
   (-> m
       (update :specs   conj results-spec)
       (update :mutations assoc results-spec {:resolver resolver
                                              :mutation-spec mutation-spec}))))

(defn attach-schema
  "Adds an external Lacinia schema into the provided pre-compiled data structure"
  [m schema]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (update m :schemas conj schema))

(defn- generate-root-objects
  "Generates root objects (mutations and queries) from the pre-compiled data structure"
  [m access-key id]
  (->> m
       (map (fn [[k v]]
              (let [objects (:objects (leona-schema/transform (get v access-key)))
                    args-object (util/clj-name->gql-name (get v access-key))]
                (hash-map (util/clj-name->gql-name k)
                          {:type (util/clj-name->gql-name k)
                           :input-objects (dissoc objects args-object)
                           :args (get-in objects [args-object :fields])
                           :resolve (wrap-resolver id (:resolver v) (get v access-key) k)}))))
       (apply merge)))

(defn- extract-input-objects
  "Extracts an input object map out of the result of generate-root-objects."
  [m]
  (apply merge (map :input-objects (vals m))))

(defn- dissoc-input-objects
  "Remove input objects from individual query/mutation maps."
  [m]
  (into {} (map (fn [[k v]] [k (dissoc v :input-objects)]) m)))

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
               (reduce-kv (fn [a k _] (inject-field-resolver a k frs)) d d)
               d)) %)))

(defn add-external-schemas
  [generated schemas]
  (if (not-empty schemas)
    (reduce (partial merge-with merge) generated schemas)
    generated))

;;;;;

(defn transform-input-object-key [k]
  "Adds a suffix '_input_' to the provided keyword k"
  (-> k
      name
      (str "_input")
      keyword))

(defn transform-input-object-keys [m]
  "Given the map m, transforms all its keys using the transform-input-object-key function"
  (->> m
       (map (fn [[k v]]
              [(transform-input-object-key k)
               v]))
       (into {})))

(defn replace-input-objects [m input-objects]
  "Given a map and map of input objects, replaces instances of input-object types with their transformed form"
  (walk/postwalk
    (fn replace-input-object-types [d]
      (if-let [match (some #(when (or (= d {:type %})
                                      (= d {:type (list 'non-null %)}))
                              %)
                           (keys input-objects))]
        (walk/postwalk
          (fn replace-matched-type [n]
            (if (= n match)
              (transform-input-object-key n)
              n))
          d)
        d))
    m))

(defn generate
  "Takes pre-compiled data structure and converts it into a Lacinia schema"
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (let [queries       (generate-root-objects (:queries m) :query-spec :query)
        mutations     (generate-root-objects (:mutations m) :mutation-spec :mutation)
        input-objects (merge (extract-input-objects queries) (extract-input-objects mutations))]
    (cond-> (apply leona-schema/combine (:specs m))
      queries                          (assoc :queries (-> queries
                                                           (dissoc-input-objects)
                                                           (replace-input-objects input-objects)))
      mutations                        (assoc :mutations (-> mutations
                                                             (dissoc-input-objects)
                                                             (replace-input-objects input-objects)))
      input-objects                    (assoc :input-objects (transform-input-object-keys input-objects))
      (not-empty (:field-resolvers m)) (inject-field-resolvers (:field-resolvers m)))))

(defn compile
  "Generates a Lacinia schema from pre-compiled data structure and compiles it."
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (let [generated (-> m
                      (generate)
                      (add-external-schemas (:schemas m)))]
    {:compiled   (lacinia-schema/compile generated)
     :generated  generated
     :middleware (:middleware m)}))

(defn execute
  "Executes Lacinia commands; adds middleware into the context which is required by the resolver wrapper"
  ([m query]
   (execute m query nil {}))
  ([m query variables ctx]
   {:pre [(s/valid? ::compiled-data m)]}
   (lacinia/execute (:compiled m) query variables (merge ctx (select-keys m [:middleware])))))
