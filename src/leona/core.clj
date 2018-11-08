(ns leona.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.spec.alpha :as s]
            [com.walmartlabs.lacinia.schema :as schema]
            [leona.lacinia.schema :as leona-schema]))

(s/def :leona.pcd/query-spec keyword?)
(s/def :leona.pcd/resolver fn?)
(s/def :leona.pcd/query (s/keys :req [:leona.pcd/resolver
                                      :leona.pcd/query-spec]))
(s/def :leona.pcd/specs (s/coll-of keyword? :kind set))
(s/def :leona.pcd/queries (s/map-of keyword? :leona.pcd/query))
(s/def :leona/pre-compiled-data (s/keys :req [:leona.pcd/specs
                                              :leona.pcd/queries]))

(defn create [] {:specs #{}
                 :queries {}})

(defn attach-query
  ([m resolver]
   ;; infer specs from fdef
   )
  ([m query-spec resolver results-spec]
   {:pre [(s/explain-data :leona/pre-compiled-data m)]}
   (-> m
       (update :specs conj results-spec)
       (update :queries assoc results-spec {:resolver resolver
                                            :query-spec query-spec}))))

(defn generate
  [m]
  {:pre [(s/explain-data :leona/pre-compiled-data m)]}
  (-> (apply leona-schema/combine (:specs m))
      (update :queries WIP)))

(defn compile
  [m]
  (-> m
      (generate)
      (schema/compile)))
