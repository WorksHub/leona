(ns leona.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [com.walmartlabs.lacinia.schema :as lacinia-schema]
            [com.walmartlabs.lacinia.util :as lacinia-util]
            [leona.core :as leona]
            [leona.schema :as leona-schema]
            [leona.test-spec :as test]
            [leona.util :as util]))

(deftest external-compile-test
  (is (-> (leona-schema/combine ::test/human
                                ::test/droid)
          (lacinia-schema/compile))))

(deftest create-test
  (let [droid-resolver (fn [ctx query value])
        droid-mutator  (fn [ctx query value])
        human-resolver (fn [ctx query value])
        middleware     (fn [handler ctx query value])]
    (is (= {:specs #{::test/droid}
            :middleware [middleware]
            :queries {::test/droid {:resolver droid-resolver
                                    :query-spec ::test/droid-query}}
            :mutations {::test/droid {:resolver droid-mutator
                                      :mutation-spec ::test/droid-mutation}}
            :field-resolvers {::test/owner {:resolver human-resolver}}
            :schemas []}
           (-> (leona/create)
               (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
               (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
               (leona/attach-field-resolver ::test/owner human-resolver)
               (leona/attach-middleware middleware))))))

(defn droid-resolver
  [ctx query value]
  (case (:id query)
    1001 {:primary-functions ["courier" "fixer"]
          :id 1001
          :name "R2D2"
          ::test/appears-in [:NEWHOPE :EMPIRE :JEDI]
          :operational? true}
    1003 {:foo "bar"}
    nil))

(defn droid-mutator
  [ctx {:keys [primary-functions]} value]
  {:primary-functions primary-functions
   :id 1001
   :name "R2D2"
   ::test/appears-in [:NEWHOPE :EMPIRE :JEDI]
   :operational? true})

(deftest generate-query-test
  (is (= {:Droid
          {:type :Droid,
           :args {:Id {:type '(non-null Int)},
                  (util/clj-name->qualified-gql-name ::test/appears-in) {:type '(list :Episode)}}}}
         (-> (leona/create)
             (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
             (leona/generate)
             :queries
             (update :Droid dissoc :resolve))))) ;; remove resolver because it gets wrapped

(deftest generate-mutation-test
  (is (= {:Droid
          {:type :Droid,
           :args {:Id {:type '(non-null Int)},
                  :PrimaryFunctions {:type '(list String)}}}}
         (-> (leona/create)
             (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
             (leona/generate)
             :mutations
             (update :Droid dissoc :resolve)))))

(deftest query-valid-test
  (let [appears-in-str (name (util/clj-name->qualified-gql-name ::test/appears-in))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema
                              (format "query { Droid(Id: 1001, %s: NEWHOPE) { Name, Operational_QMARK_, %s }}"
                                      appears-in-str
                                      appears-in-str))]
    (is (= "R2D2" (get-in result [:data :Droid :Name])))
    (is (= true (get-in result [:data :Droid :Operational_QMARK_])))
    (is (= '(:NEWHOPE :EMPIRE :JEDI) (get-in result [:data :Droid (keyword appears-in-str)])))))

(deftest query-invalid-gql-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query { Droid(Id: \"hello\") { Name }}")] ;; id is NaN
    (is (:errors result))
    (is (= {:field :Droid :argument :Id :value "hello" :type-name :Int} (-> result :errors first :extensions)))))

(deftest query-invalid-query-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query { Droid(Id: 1002) { Name }}")] ;; id is even
    (is (:errors result))
    (is (= :invalid-query (-> result :errors first :extensions :key)))))

(deftest query-invalid-result-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query { Droid(Id: 1003) { Name }}")]
    (is (:errors result))
    (is (= :invalid-query-result (-> result :errors first :extensions :key)))))

;;;;;;;

(deftest mutation-valid-test
  (let [appears-in-str (name (util/clj-name->qualified-gql-name ::test/appears-in))
        compiled-schema (-> (leona/create)
                            (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
                            (leona/compile))
        result (leona/execute compiled-schema
                              "mutation { Droid(Id: 1001, PrimaryFunctions: [\"beep\"]) { Name, Operational_QMARK_, PrimaryFunctions }}")]
    (is (= "R2D2"   (get-in result [:data :Droid :Name])))
    (is (= true     (get-in result [:data :Droid :Operational_QMARK_])))
    (is (= ["beep"] (get-in result [:data :Droid :PrimaryFunctions])))))

(deftest mutation-invalid-gql-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
                            (leona/compile))
        result (leona/execute compiled-schema "mutation { Droid(Id: \"hello\") { Name }}")] ;; id is NaN
    (is (:errors result))
    (is (= {:field :Droid :argument :Id :value "hello" :type-name :Int} (-> result :errors first :extensions)))))

(deftest mutation-invalid-mutation-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
                            (leona/compile))
        result (leona/execute compiled-schema "mutation { Droid(Id: 1002) { Name }}")] ;; id is even
    (is (:errors result))
    (is (= :invalid-mutation (-> result :errors first :extensions :key)))))

(deftest mutation-invalid-result-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
                            (leona/compile))
        result (leona/execute compiled-schema "mutation { Droid(Id: 1003) { Name }}")]
    (is (:errors result))
    (is (= :invalid-mutation-result (-> result :errors first :extensions :key)))))

;;;;;

(deftest input-object-test
  (s/def ::num int?)
  (s/def ::nums (s/coll-of ::num))
  (s/def ::input (s/keys :req-un [::num ::nums]))
  (s/def ::args (s/keys :req-un [::input]))
  (s/def ::test (s/keys :req-un [::num]))
  (s/def ::test-query (s/keys :req-un [::input]))
  (let [resolver (fn [ctx query value]
                   (let [{:keys [num nums]} (:input query)]
                     {:num (apply + num nums)}))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test-query ::test resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query Test($Input: Input_input!) { Test(Input: $Input) { Num }}" {:Input {:Num 1, :Nums [2 3]}} {})]
    (is (= 6 (get-in result [:data :Test :Num])))))

;;;;;

(deftest input-object-using-input-object-test
  (s/def ::num int?)
  (s/def ::nums (s/coll-of ::num))
  (s/def ::input (s/keys :req-un [::num ::nums]))
  (s/def ::args (s/keys :req-un [::input]))
  (s/def ::test (s/keys :req-un [::num]))
  (s/def ::test-query (s/keys :req-un [::input]))
  (s/def ::test-query2 (s/keys :req-un [::test-query]))
  (let [resolver (fn [ctx query value]
                   (let [{:keys [num nums]} (:input (:test-query query))]
                     {:num (apply + num nums)}))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test-query2 ::test resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query Test($TestQuery: TestQuery_input!) { Test(TestQuery: $TestQuery) { Num }}"
                              {:TestQuery {:Input {:Num 1, :Nums [2 3]}}} {})]
    (is (= 6 (get-in result [:data :Test :Num])))))


;;;;;

(deftest input-objects-nested-list
  (s/def ::num int?)
  (s/def ::nums (s/coll-of ::num))
  (s/def ::input (s/keys :req-un [::num ::nums]))
  (s/def ::args (s/keys :req-un [::input]))
  (s/def ::test (s/keys :req-un [::num]))
  (s/def ::inputs (s/coll-of ::input))
  (s/def ::test-query (s/keys :req-un [::inputs]))
  (let [resolver (fn [ctx query value]
                   (let [{:keys [num nums]} (first (:inputs query))]
                     {:num (apply + num nums)}))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test-query ::test resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query Test($Inputs: [Input_input!]!) { Test(Inputs: $Inputs) { Num }}" {:Inputs [{:Num 1, :Nums [2 3]}]} {})]
    (is (= 6 (get-in result [:data :Test :Num])))))



;;;;;

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
                   (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                   (leona/attach-middleware mw-fn2)
                   (leona/compile)
                   (leona/execute "query { Droid(Id: 1001) { Name }}"))]
    (is (= "R2D2" (get-in result [:data :Droid :Name])))
    (is (= 25 @test-atom))))

(deftest middleware-bail-test
  (let [mw-fn1 (fn [handler ctx query value]
                 (leona/error {:key :auth-failed}))
        result (-> (leona/create)
                   (leona/attach-middleware mw-fn1)
                   (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                   (leona/compile)
                   (leona/execute "query { Droid(Id: 1001) { Name }}"))]
    (is (:errors result))
    (is (= {:key :auth-failed, :arguments {:Id 1001}} (-> result :errors first :extensions)))))

;;;;;;;

(deftest field-resolver-opt-test
  (let [human {:home-planet "Naboo"
               :id 123145
               :name "Jack Solo"
               :appears-in #{:JEDI}}
        human-resolver (fn [ctx query value] human)
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/attach-field-resolver ::test/owner human-resolver) ;; 'owner' is the field
                            (leona/compile))
        result (leona/execute compiled-schema "query { Droid(Id: 1001) { Name, Operational_QMARK_, Owner {Id} }}")]
    (is (= "R2D2" (get-in result [:data :Droid :Name])))
    (is (= (:id human) (get-in result [:data :Droid :Owner :Id])))))


(deftest field-resolver-included-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::test (s/keys :opt-un [::b]))
  (s/def ::test-query (s/keys :opt-un [::a]))
  (let [r (-> (leona/create)
              (leona/attach-query ::test-query ::test droid-resolver)
              (leona/attach-field-resolver ::b (constantly {:A 123}))
              (leona/generate))]
    (is (get-in r [:objects :Test :fields :B :resolve]))))

(deftest field-resolver-coll-included-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c (s/coll-of ::b))
  (s/def ::test (s/keys :opt-un [::c]))
  (s/def ::test-query (s/keys :opt-un [::a]))
  (let [r (-> (leona/create)
              (leona/attach-query ::test-query ::test droid-resolver)
              (leona/attach-field-resolver ::c (constantly {:A 123}))
              (leona/generate))]
    (is (get-in r [:objects :Test :fields :C :resolve]))))

(deftest field-resolver-coll-included-in-ref-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c (s/coll-of ::b))
  (s/def ::test (s/keys :opt-un [::c]))
  (s/def ::test-query (s/keys :opt-un [::a]))
  (let [r (-> (leona/create)
              (leona/attach-query ::test-query ::test droid-resolver)
              (leona/attach-field-resolver ::a (constantly {:A 123}))
              (leona/generate))]
    (is (get-in r [:objects :B :fields :A :resolve]))))

;;;;;;;

(deftest basic-merge-schema-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::test (s/keys :opt-un [::b]))
  (s/def ::test-query (s/keys :opt-un [::a]))
  (let [r (-> (leona/create)
              (leona/attach-query ::test-query ::test droid-resolver)
              (leona/attach-schema {:objects {:foo {:fields {:bar {:type 'String}}}}})
              (leona/compile))]
    (is (get-in r [:generated :objects :foo :fields :bar]))))

(deftest merge-schema-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::test (s/keys :opt-un [::b]))
  (s/def ::test-query (s/keys :opt-un [::a]))
  (def schema
    (-> {:queries {:alice {:type :foo, :args {:bob {:type 'Int}}, :resolve :my-query}}
         :objects {:foo {:fields {:bar {:type 'String}}}}}
        (lacinia-util/attach-resolvers {:my-query identity})))
  (let [r (-> (leona/create)
              (leona/attach-query ::test-query ::test droid-resolver)
              (leona/attach-schema schema)
              (leona/compile))]
    (is (= identity (get-in r [:generated :queries :alice :resolve])))))
