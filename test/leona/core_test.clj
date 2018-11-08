(ns leona.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [com.walmartlabs.lacinia :refer [execute]]
            [com.walmartlabs.lacinia.schema :as lacinia-schema]
            [leona.core :as leona]
            [leona.lacinia.schema :as leona-schema]
            [leona.test-spec :as test]))

(deftest external-compile-test
  (is (-> (leona-schema/combine ::test/human
                                ::test/droid)
          (lacinia-schema/compile))))

(deftest create
  (let [droid-resolver (fn [ctx query value])]
    (is (= (-> (leona/create)
               (leona/attach-query ::test/droid droid-resolver))
           {:specs #{::test/droid}
            :queries {::test/droid droid-resolver}}))))

(deftest generate

  (let [droid-resolver (fn [ctx query value])]
    (println (-> (leona/create)
                 (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                 (leona/generate))
             )))

(deftest query-test
  (let [droid-resolver (fn [ctx query value])
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                            ;;(leona/attach-query `human-resolver)
                            (leona/compile))
        result 123 #_(execute compiled-schema
                              "query { human(id: \"1001\") { name }}"
                              nil nil)]
    (println result)))
