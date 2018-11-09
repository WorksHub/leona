(ns leona.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [com.walmartlabs.lacinia.schema :as lacinia-schema]
            [leona.core :as leona]
            [leona.schema :as leona-schema]
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

(defn droid-resolver
  [ctx query value]
  (when (= 1001 (:id query))
    {:primary-functions ["courier" "fixer"]
     :id 1001
     :name "R2D2"
     :appears-in [:NEWHOPE :EMPIRE :JEDI]
     :operational? true}))

(deftest generate-test
  (is (= {:droid
          {:type :droid,
           :args {:id {:type '(non-null Int)},
                  :name {:type 'String}}
           :resolve droid-resolver}}
         (-> (leona/create)
             (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
             (leona/generate)
             :queries))))

(deftest query-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                            (leona/compile))
        result (execute compiled-schema "query { droid(id: \"1001\") { name }}" nil nil)]
    (is (= "R2D2" (get-in result [:data :droid :name])))))

(deftest middleware-test
  (let [auth-fn (fn [ctx query value handler]
                  (handler ctx query value))
        result (-> (leona/create)
                   (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                   (leona/attach-middleware auth-fn)
                   (leona/compile)
                   (leona/execute "query { droid(id: \"1001\") { name }}"))]
    (is (= "R2D2" (get-in result [:data :droid :name])))))
