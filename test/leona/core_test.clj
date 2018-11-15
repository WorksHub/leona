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
    (is (= {:specs #{::test/droid}
            :middleware []
            :queries {::test/droid {:resolver droid-resolver
                                    :query-spec ::test/droid-query}}}
           (-> (leona/create)
               (leona/attach-query ::test/droid-query droid-resolver ::test/droid))))))

(defn droid-resolver
  [ctx query value]
  (case (:id query)
    1001 {:primary-functions ["courier" "fixer"]
          :id 1001
          :name "R2D2"
          :appears-in [:NEWHOPE :EMPIRE :JEDI]
          :operational? true}
    1003 {:foo "bar"}
    nil))

(deftest generate-test
  (is (= {:droid
          {:type :droid,
           :args {:id {:type '(non-null Int)},
                  :name {:type 'String}}}}
         (-> (leona/create)
             (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
             (leona/generate)
             :queries
             (update :droid dissoc :resolve))))) ;; remove resolver because it gets wrapped

(deftest query-valid-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                            (leona/compile))
        result (leona/execute compiled-schema "query { droid(id: 1001, appears_in: NEWHOPE) { name, operational_QMARK_, appears_in }}")] ;; id is odd
    (is (= "R2D2" (get-in result [:data :droid :name])))
    (is (= true   (get-in result [:data :droid :operational_QMARK_])))
    (is (= '(:NEWHOPE :EMPIRE :JEDI) (get-in result [:data :droid :appears_in])))))

(deftest query-invalid-gql-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                            (leona/compile))
        result (leona/execute compiled-schema "query { droid(id: \"hello\") { name }}")] ;; id is NaN
    (is (:errors result))
    (is (= {:field :droid :argument :id :value "hello" :type-name :Int} (-> result :errors first :extensions)))))

(deftest query-invalid-query-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                            (leona/compile))
        result (leona/execute compiled-schema "query { droid(id: 1002) { name }}")] ;; id is even
    (is (:errors result))
    (is (= :invalid-query (-> result :errors first :extensions :key)))))

(deftest query-invalid-result-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                            (leona/compile))
        result (leona/execute compiled-schema "query { droid(id: 1003) { name }}")]
    (is (:errors result))
    (is (= :invalid-result (-> result :errors first :extensions :key)))))

(deftest middleware-test
  (let [test-atom (atom 10)
        mw-fn1 (fn [handler ctx query value]
                 (swap! test-atom (partial * 2))
                 (handler))
        mw-fn2 (fn [handler ctx query value]
                 (swap! test-atom (partial + 5))
                 (handler))
        result (-> (leona/create)
                   (leona/attach-middleware mw-fn1)
                   (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                   (leona/attach-middleware mw-fn2)
                   (leona/compile)
                   (leona/execute "query { droid(id: 1001) { name }}"))]
    (is (= "R2D2" (get-in result [:data :droid :name])))
    (is (= 25 @test-atom))))

(deftest middleware-bail-test
  (let [mw-fn1 (fn [handler ctx query value]
                 (leona/error {:key :auth-failed}))
        result (-> (leona/create)
                   (leona/attach-middleware mw-fn1)
                   (leona/attach-query ::test/droid-query droid-resolver ::test/droid)
                   (leona/compile)
                   (leona/execute "query { droid(id: 1001) { name }}"))]
    (is (:errors result))
    (is (= {:key :auth-failed, :arguments {:id "1001"}} (-> result :errors first :extensions)))))
