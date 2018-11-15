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
(s/def ::middleware (s/coll-of fn? :kind set))
(s/def ::specs (s/coll-of keyword? :kind set))
(s/def ::queries (s/map-of keyword? ::query))
(s/def ::mutations (s/map-of keyword? ::mutation))
(s/def ::pre-compiled-data (s/keys :req-un [::specs
                                            ::queries
                                            ::mutations
                                            ::middleware]))
(s/def ::compiled map?)
(s/def ::compiled-data (s/keys :req-un [::compiled
                                        ::middleware]))

(defn build-middleware
  [resolver middleware args]
  (reduce (fn [a f] (apply partial (concat [f a] args))) resolver (reverse middleware)))

(defn error
  [{key :key message :message :as error-map}]
  (let [error-map (merge {:message (or message (name key))} error-map)]
    (lacinia-resolve/resolve-as nil error-map)))

(defn format-result
  [m]
  (cske/transform-keys
   (comp keyword
         util/replace-punctuation
         csk/->snake_case
         name) m))

(defn format-input
  [m]
  (cske/transform-keys
   (comp keyword
         csk/->kebab-case
         util/replace-placeholders
         name) m))

(defn wrap-resolver
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

(defn create [] {:specs #{}
                 :queries {}
                 :mutations {}
                 :middleware []})

(defn attach-middleware
  [m middleware-fn]
  (update m :middleware conj middleware-fn))

(defn attach-query
  ([m resolver]
   ;; TODO infer specs from fdef
   )
  ([m query-spec resolver results-spec]
   {:pre [(s/valid? ::pre-compiled-data m)]}
   (-> m
       (update :specs   conj results-spec)
       (update :queries assoc results-spec {:resolver resolver
                                            :query-spec query-spec}))))

(defn attach-mutation
  ([m resolver]
   ;; TODO infer specs from fdef
   )
  ([m mutation-spec resolver results-spec]
   {:pre [(s/valid? ::pre-compiled-data m)]}
   (-> m
       (update :specs   conj results-spec)
       (update :mutations assoc results-spec {:resolver resolver
                                              :mutation-spec mutation-spec}))))

(defn- generate-root-objects
  [m access-key id]
  (->> m
       (map (fn [[k v]]
              (hash-map (util/clj-name->gql-name k)
                        {:type (util/clj-name->gql-name k)
                         :args (get-in (leona-schema/transform (get v access-key))
                                       [:objects (util/clj-name->gql-name (get v access-key)) :fields])
                         :resolve (wrap-resolver id (:resolver v) (get v access-key) k)})))
       (apply merge)))

(defn generate
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (cond-> (apply leona-schema/combine (:specs m))
    (not-empty (:queries m))   (assoc :queries   (generate-root-objects (:queries m)   :query-spec    :query))
    (not-empty (:mutations m)) (assoc :mutations (generate-root-objects (:mutations m) :mutation-spec :mutation))))

(defn compile
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  {:compiled (-> m
                 (generate)
                 (lacinia-schema/compile))
   :middleware (:middleware m)})

(defn execute
  [m query]
  {:pre [(s/valid? ::compiled-data m)]}
  (lacinia/execute (:compiled m) query nil (select-keys m [:middleware])))
