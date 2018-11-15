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
(s/def ::resolver fn?)
(s/def ::query (s/keys :req-un [::resolver
                                ::query-spec]))
(s/def ::middleware (s/coll-of fn? :kind set))
(s/def ::specs (s/coll-of keyword? :kind set))
(s/def ::queries (s/map-of keyword? ::query))
(s/def ::pre-compiled-data (s/keys :req-un [::specs
                                            ::queries
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

(defn format-query
  [m]
  (cske/transform-keys
   (comp keyword
         csk/->kebab-case
         util/replace-placeholders
         name) m))

(defn wrap-resolver
  [resolver-fn query-spec output-spec]
  (fn [ctx query value]
    (let [formatted-query (format-query query)]
      (if-not (s/valid? query-spec formatted-query)
        (error {:key :invalid-query
                :args (s/explain-data query-spec formatted-query)
                :message (str "The query didn't conform to the internal spec: " query-spec)})
        (let [resolver (partial resolver-fn ctx formatted-query value)
              with-middleware (build-middleware resolver (:middleware ctx) [ctx formatted-query value])
              result (with-middleware)]
          (cond
            (instance? com.walmartlabs.lacinia.resolve.ResolverResultImpl result) result
            (s/valid? output-spec result) (format-result result)
            :else (error {:key :invalid-result
                          :args (s/explain-data output-spec result)
                          :message (str "The result didn't conform to the internal spec: " output-spec)})))))))

(defn create [] {:specs #{}
                 :queries {}
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

(defn generate-queries
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (->> (:queries m)
       (map (fn [[k v]]
              (hash-map (util/clj-name->gql-name k)
                        {:type (util/clj-name->gql-name k)
                         :args (get-in (leona-schema/transform (:query-spec v))
                                       [:objects (util/clj-name->gql-name (:query-spec v)) :fields])
                         :resolve (wrap-resolver (:resolver v) (:query-spec v) k)})))
       (apply merge)))

(defn generate
  [m]
  {:pre [(s/valid? ::pre-compiled-data m)]}
  (-> (apply leona-schema/combine (:specs m))
      (assoc :queries (generate-queries m))))

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
