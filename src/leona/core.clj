(ns leona.core
  (:refer-clojure :exclude [compile])
  (:require [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
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
(s/def ::input-objects (s/coll-of keyword? :kind set))
(s/def ::queries (s/map-of keyword? ::query))
(s/def ::mutations (s/map-of keyword? ::mutation))
(s/def ::field-resolvers (s/map-of keyword? ::field-resolver))
(s/def ::type-aliases (s/map-of keyword? keyword?))
(s/def ::parse fn?)
(s/def ::serialize fn?)
(s/def ::scalar-map (s/keys :req-un [::parse
                                     ::serialize]))
(s/def ::custom-scalars (s/map-of keyword? ::scalar-map))
(s/def ::pre-compiled-data (s/keys :req-un [::specs
                                            ::input-objects
                                            ::queries
                                            ::mutations
                                            ::field-resolvers
                                            ::middleware
                                            ::type-aliases
                                            ::custom-scalars]))
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

(defn- enum-spec->values
  "Attempts to resolve set values from a given spec. If the spec is NOT a set, will just return nil."
  [spec]
  (when (keyword? spec)
    (let [form (s/form spec)]
      (cond (set? form) form
            (and (sequential? form)
                 (= (first form) 'clojure.spec.alpha/coll-of))
            (enum-spec->values (second form))))))

(defn format-enums
  [f m spec]
  (reduce
    (fn [a spec-key]
      (let [s (enum-spec->values spec-key)
            v (util/get* m spec-key)
            k (and v (if (contains? m spec-key)
                       spec-key
                       (util/remove-ns* spec-key)))]
        (if (and s v)
          (let [new-v (if (sequential? v)
                        (mapv #(f s %) v)
                        (f s v))]
            (assoc a k new-v))
          a))) m (util/spec-keys spec)))

(def format-input-enums
  "Format enum values in the input."
  (partial format-enums util/find-case-match))

(def format-output-enums
  "Format enum values in the output."
  (partial format-enums (fn [_s v] (util/clj-name->qualified-gql-enum-name v))))

(defn format-result
  "Format the results of a resolver"
  [m spec]
  (->> (format-output-enums m spec)
       (cske/transform-keys util/clj-name->qualified-gql-name)))

(defn format-input
  "Format the input into a resolver"
  [m spec]
  (-> (cske/transform-keys util/gql-name->clj-name m)
      (format-input-enums spec)))

(defn wrap-resolver
  "Used to wrap resolver fns provided by the user. This adds re-formatting in both directions and spec validation"
  [id resolver-fn input-spec result-spec]
  (fn [ctx input value]
    (let [formatted-input (format-input input input-spec)]
      (if-not (s/valid? input-spec formatted-input)
        (error {:key (keyword (str "invalid-" (name id)))
                :args (s/explain-data input-spec formatted-input)
                :message (str "The " (name id) " input didn't conform to the internal spec: " input-spec)})
        (let [resolver (-> (partial resolver-fn ctx formatted-input value)
                           (build-middleware (:middleware ctx) [ctx formatted-input value]))
              result (resolver)]
          (cond
            (instance? com.walmartlabs.lacinia.resolve.ResolverResultImpl result) result
            (s/valid? result-spec result) (format-result result result-spec)
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
   :type-aliases {}
   :middleware []
   :schemas []
   :input-objects #{}
   :custom-scalars {}})

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

(defn attach-object
  "Adds an object (not referred to by a query, mutation, or field resolver) into the provided pre-compiled data structure"
  [m object-spec & {:keys [input?]}]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (cond-> m
    true (update :specs conj object-spec)
    input? (update :input-objects conj object-spec)))

(defn attach-query
  "Adds a query resolver into the provided pre-compiled data structure.
  In the form [m resolver] I expect `:leona/query-spec` and `:leona/results-spec`
  as metadata on `resolver`.
  Specs should be keywords, see [[::query-spec]]."
  ([m resolver]
   (let [{:leona/keys [query-spec results-spec]} (meta resolver)
         _ (assert query-spec)
         _ (assert results-spec)]
     (attach-query m query-spec results-spec resolver)))
  ([m query-spec results-spec resolver]
   {:pre [(s/valid? ::pre-compiled-data m)]}
   (-> m
       (update :specs   conj results-spec)
       (update :queries assoc results-spec {:resolver resolver
                                            :query-spec query-spec}))))

(defn attach-mutation
  "Adds a mutation resolver fn into the provided pre-compiled data structure
  In the form [m resolver] I expect `:leona/mutation-spec` and `:leona/results-spec`
  as metadata on `resolver`.
  Specs should be keywords, see [[::mutation-spec]]."
  ([m resolver]
   (let [{:leona/keys [mutation-spec results-spec]} (meta resolver)
         _ (assert mutation-spec)
         _ (assert results-spec)]
     (attach-mutation m mutation-spec results-spec resolver)))
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
  [m access-key id opts]
  (->> m
       (map (fn [[k v]]
              (let [{:keys [objects enums]} (leona-schema/transform (get v access-key) opts)
                    args-object             (util/clj-name->gql-object-name (get v access-key))]
                (hash-map (util/clj-name->gql-name k)
                          (merge {:type          (leona-schema/spec-name-or-alias k opts)
                                  :input-objects (dissoc objects args-object)
                                  :args          (get-in objects [args-object :fields])
                                  :resolve       (wrap-resolver id (:resolver v) (get v access-key) k)}
                                 (when (not-empty enums)
                                   {:enums enums}))))))
       (apply merge)))

(defn- extract-all
  "Extracts k out of the result of generate-root-objects."
  [m k]
  (apply merge (map k (vals m))))

(defn- dissoc-all
  "Remove key from from individual query/mutation maps."
  [m ky]
  (into {} (map (fn [[k v]] [k (dissoc v ky)]) m)))

(defn- inject-field-resolver
  "Finds a field resolver from the provided collection and injects it into the appropriate place (object field)"
  [m field frs]
  (let [field-spec (get-in m [field :spec])]
    (if-let [fr (get frs field-spec)]
      (assoc-in m [field :resolve] (wrap-resolver :field (:resolver fr) any? field-spec))
      m)))

(s/def ::field-with-type
  (s/map-of keyword? (s/and map? #(contains? % :type))))

(defn inject-field-resolvers
  "Walks a set of objects, attempting to inject field resolvers into certain types"
  [m frs]
  (update
    m :objects
    #(walk/postwalk
       (fn [d]
         (if (s/valid? ::field-with-type d)
           (reduce-kv (fn [a k _] (inject-field-resolver a k frs)) d d)
           d)) %)))

(defn add-external-schemas
  [generated schemas]
  (if (not-empty schemas)
    (reduce (partial merge-with merge) generated schemas)
    generated))

(defn attach-type-alias
  "Attach an alias to a spec that's a type. All type instances of that spec name will be replaced with the alias. This can be used to avoid conflicts."
  [m spec alias]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (assoc-in m [:type-aliases spec] alias))

(defn attach-custom-scalar
  "Attaches a custom scalar to the schema. See https://lacinia.readthedocs.io/en/latest/custom-scalars.html"
  [m spec {:keys [_parse _serialize] :as sm}]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (assoc-in m [:custom-scalars spec] sm))

;;;;;

(defn transform-input-object-key
  "Adds a suffix 'input' to the provided keyword k"
  [k]
  (let [k (if (str/starts-with? (str k) ":")
            (-> k str (subs 1))
            (str k))])
  (util/clj-name->qualified-gql-object-name (str k " input")))

(defn transform-input-object-keys
  "Given the map m, transforms all its keys using the transform-input-object-key function"
  [m]
  (->> m
       (map (fn [[k v]]
              [(transform-input-object-key k)
               v]))
       (into {})))

(defn replace-input-objects
  "Given a map and map of input objects, replaces instances of input-object types with their transformed form"
  [m input-objects]
  (walk/postwalk
    (fn replace-input-object-types [d]
      (if (and (map? d)
               (contains? d :type))
        (if (keyword? (:type d))
          (if (some #(= (:type d) %) (keys input-objects))
            (update d :type transform-input-object-key)
            d)
          (walk/postwalk
            (fn replace-matched-type [n] ;; {:type ;..... }
              (if (some #(= n %) (keys input-objects))
                (transform-input-object-key n)
                n))
            d))
        d))
    m))

(defn inject-custom-scalars
  "Add custom scalars to the Lacinia schema"
  [m sms]
  (assoc m :scalars (reduce-kv (fn [a k v] (assoc a (util/clj-name->gql-object-name k) v)) {} sms)))

(defn generate
  "Takes pre-compiled data structure and converts it into a Lacinia schema"
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (let [opts            (select-keys m [:type-aliases :custom-scalars])
        queries         (generate-root-objects (:queries m) :query-spec :query opts)
        mutations       (generate-root-objects (:mutations m) :mutation-spec :mutation opts)
        input-objects   (not-empty (apply merge
                                          (extract-all queries   :input-objects)
                                          (extract-all mutations :input-objects)
                                          (map (comp :objects #(leona-schema/transform % opts)) (:input-objects m))))
        enums           (not-empty (apply merge
                                          (extract-all queries   :enums)
                                          (extract-all mutations :enums)))
        field-resolvers (not-empty (:field-resolvers m))
        custom-scalars  (not-empty (:custom-scalars m))]
    (cond-> (apply leona-schema/combine-with-opts opts (:specs m))
            queries         (assoc :queries (-> queries
                                                (dissoc-all :enums)
                                                (dissoc-all :input-objects)
                                                (replace-input-objects input-objects)))
            mutations       (assoc :mutations (-> mutations
                                                  (dissoc-all :enums)
                                                  (dissoc-all :input-objects)
                                                  (replace-input-objects input-objects)))
            input-objects   (assoc :input-objects (-> input-objects
                                                      (transform-input-object-keys)
                                                      (replace-input-objects input-objects)))
            enums           (update :enums merge enums)
            field-resolvers (inject-field-resolvers field-resolvers)
            custom-scalars  (inject-custom-scalars custom-scalars))))

(defn compile
  "Generates a Lacinia schema from pre-compiled data structure and compiles it.

  Compile options:

  :default-field-resolver

  : Passed to Lacinia. A function that accepts a field name (as a keyword) and converts it into the
    default field resolver; see Lacinia src"
  ([m]
   (compile m nil))
  ([m opts]
   {:pre [(s/valid? ::pre-compiled-data m)]}
   (let [generated (-> m
                       (generate)
                       (add-external-schemas (:schemas m)))]
     {:compiled   (lacinia-schema/compile generated opts)
      :generated  generated
      :middleware (:middleware m)})))

(defn execute
  "Executes Lacinia commands; adds middleware into the context which is required by the resolver wrapper"
  ([m query]
   (execute m query nil {}))
  ([m query variables ctx]
   {:pre [(s/valid? ::compiled-data m)]}
   (lacinia/execute (:compiled m) query variables (merge ctx (select-keys m [:middleware])))))
