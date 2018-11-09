(ns leona.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.spec.alpha :as s]
            [com.walmartlabs.lacinia :as lacinia]
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

(defn wrap-resolver
  [resolver-fn]
  (fn [ctx query value]
    (println ctx) <<<<<<----- mIDDLEWARE
    (resolver-fn ctx query value)))

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
                         :args (get-in (leona-schema/transform (:query-spec v)) [:objects (util/clj-name->gql-name (:query-spec v)) :fields])
                         :resolve (wrap-resolver (:resolver v))})))
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
