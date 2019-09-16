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
            :input-objects #{}
            :field-resolvers {::test/owner {:resolver human-resolver}}
            :schemas []
            :type-aliases {}
            :custom-scalars {}}
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
          ::test/appears-in [:new-hope :empire :jedi]
          :operational? true}
    1003 {:foo "bar"}
    nil))

(defn droid-mutator
  [ctx {:keys [primary-functions]} value]
  {:primary-functions primary-functions
   :id 1001
   :name "R2D2"
   ::test/appears-in [:new-hope :empire :jedi]
   :operational? true})

(deftest generate-query-test
  (is (= {:droid
          {:type :Droid,
           :args {:id {:type '(non-null Int)},
                  (util/clj-name->qualified-gql-name ::test/appears-in) {:type '(list :Episode)}}}}
         (-> (leona/create)
             (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
             (leona/generate)
             :queries
             (update :droid dissoc :resolve))))) ;; remove resolver because it gets wrapped

(deftest generate-mutation-test
  (is (= {:droid
          {:type :Droid,
           :args {:id {:type '(non-null Int)},
                  :primaryFunctions {:type '(list String)}}}}
         (-> (leona/create)
             (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
             (leona/generate)
             :mutations
             (update :droid dissoc :resolve)))))

(deftest attach-object-test
  (let [schema (-> (leona/create)
                   (leona/attach-object ::test/human :input? true)
                   (leona/generate))
        expected-object '{:homePlanet {:type (non-null String)},
                          :id {:type (non-null Int)},
                          :name {:type (non-null String)},
                          :appearsIn {:type (non-null (list (non-null :Episode)))},
                          :episode {:type :Episode}}]
    (is (= (get-in schema [:objects :Human :fields]) expected-object))
    (is (= (get-in schema [:input-objects :HumanInput :fields]) expected-object))))

(deftest query-valid-test
  (let [appears-in-str (name (util/clj-name->qualified-gql-name ::test/appears-in))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema
                              (format "query { droid(id: 1001, %s: NEW_HOPE) { name, operational_QMARK_, %s }}"
                                      appears-in-str
                                      appears-in-str))]
    (is (= "R2D2" (get-in result [:data :droid :name])))
    (is (= true (get-in result [:data :droid :operational_QMARK_])))
    (is (= '(:NEW_HOPE :EMPIRE :JEDI) (get-in result [:data :droid (keyword appears-in-str)])))))

(deftest query-with-enum-valid-test
  (let [appears-in-str (name (util/clj-name->qualified-gql-name ::test/appears-in))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/another-droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema
                              (format "query { droid(id: 1001, %s: NEW_HOPE, testQueryEnum: B) { name, operational_QMARK_, %s }}"
                                      appears-in-str
                                      appears-in-str))]
    (is (= "R2D2" (get-in result [:data :droid :name])))
    (is (= true (get-in result [:data :droid :operational_QMARK_])))
    (is (= '(:NEW_HOPE :EMPIRE :JEDI) (get-in result [:data :droid (keyword appears-in-str)])))))

(deftest query-invalid-gql-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query { droid(id: \"hello\") { name }}")] ;; id is NaN
    (is (:errors result))
    (is (= {:field :droid :argument :id :value "hello" :type-name :Int} (-> result :errors first :extensions)))))

(deftest query-invalid-query-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query { droid(id: 1002) { name }}")] ;; id is even
    (is (:errors result))
    (is (= :invalid-query (-> result :errors first :extensions :key)))))

(deftest query-invalid-result-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema "query { droid(id: 1003) { name }}")]
    (is (:errors result))
    (is (= :invalid-query-result (-> result :errors first :extensions :key)))))

;;;;;;;

(deftest mutation-valid-test
  (let [appears-in-str (name (util/clj-name->qualified-gql-name ::test/appears-in))
        compiled-schema (-> (leona/create)
                            (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
                            (leona/compile))
        result (leona/execute compiled-schema
                              "mutation { droid(id: 1001, primaryFunctions: [\"beep\"]) { name, operational_QMARK_, primaryFunctions }}")]
    (is (= "R2D2"   (get-in result [:data :droid :name])))
    (is (= true     (get-in result [:data :droid :operational_QMARK_])))
    (is (= ["beep"] (get-in result [:data :droid :primaryFunctions])))))

(deftest mutation-invalid-gql-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
                            (leona/compile))
        result (leona/execute compiled-schema "mutation { droid(id: \"hello\") { name }}")] ;; id is NaN
    (is (:errors result))
    (is (= {:field :droid :argument :id :value "hello" :type-name :Int} (-> result :errors first :extensions)))))

(deftest mutation-invalid-mutation-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
                            (leona/compile))
        result (leona/execute compiled-schema "mutation { droid(id: 1002) { name }}")] ;; id is even
    (is (:errors result))
    (is (= :invalid-mutation (-> result :errors first :extensions :key)))))

(deftest mutation-invalid-result-spec-test
  (let [compiled-schema (-> (leona/create)
                            (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
                            (leona/compile))
        result (leona/execute compiled-schema "mutation { droid(id: 1003) { name }}")]
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
        result (leona/execute compiled-schema "query test($input: InputInput!) { test(input: $input) { num }}" {:input {:num 1, :nums [2 3]}} {})]
    (is (= 6 (get-in result [:data :test :num])))))

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
        result (leona/execute compiled-schema "query test($test_query: TestQueryInput!) { test(testQuery: $test_query) { num }}"
                              {:test_query {:input {:num 1, :nums [2 3]}}} {})]
    (is (= 6 (get-in result [:data :test :num])))))


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
        result (leona/execute compiled-schema "query test($inputs: [InputInput!]!) { test(inputs: $inputs) { num }}" {:inputs [{:num 1, :nums [2 3]}]} {})]
    (is (= 6 (get-in result [:data :test :num])))))



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
                   (leona/execute "query { droid(id: 1001) { name }}"))]
    (is (= "R2D2" (get-in result [:data :droid :name])))
    (is (= 25 @test-atom))))

(deftest middleware-bail-test
  (let [mw-fn1 (fn [handler ctx query value]
                 (leona/error {:key :auth-failed}))
        result (-> (leona/create)
                   (leona/attach-middleware mw-fn1)
                   (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                   (leona/compile)
                   (leona/execute "query { droid(id: 1001) { name }}"))]
    (is (:errors result))
    (is (= {:key :auth-failed, :arguments {:id 1001}} (-> result :errors first :extensions)))))

;;;;;;;

(deftest field-resolver-opt-test
  (let [human {:home-planet "Naboo"
               :id 123145
               :name "Jack Solo"
               :appears-in #{:jedi}}
        human-resolver (fn [ctx query value] human)
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/attach-field-resolver ::test/owner human-resolver) ;; 'owner' is the field
                            (leona/compile))
        result (leona/execute compiled-schema "query { droid(id: 1001) { name, operational_QMARK_, owner {id} }}")]
    (is (= "R2D2" (get-in result [:data :droid :name])))
    (is (= (:id human) (get-in result [:data :droid :owner :id])))))

(deftest field-resolver-included-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::test (s/keys :opt-un [::b]))
  (s/def ::test-query (s/keys :opt-un [::a]))
  (let [r (-> (leona/create)
              (leona/attach-query ::test-query ::test droid-resolver)
              (leona/attach-field-resolver ::b (constantly {:a 123}))
              (leona/generate))]
    (is (get-in r [:objects :Test :fields :b :resolve]))))

(deftest field-resolver-coll-included-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c (s/coll-of ::b))
  (s/def ::test (s/keys :opt-un [::c]))
  (s/def ::test-query (s/keys :opt-un [::a]))
  (let [r (-> (leona/create)
              (leona/attach-query ::test-query ::test droid-resolver)
              (leona/attach-field-resolver ::c (constantly {:a 123}))
              (leona/generate))]
    (is (get-in r [:objects :Test :fields :c :resolve]))))

(deftest field-resolver-coll-included-in-ref-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c (s/coll-of ::b))
  (s/def ::test (s/keys :opt-un [::c]))
  (s/def ::test-query (s/keys :opt-un [::a]))
  (let [r (-> (leona/create)
              (leona/attach-query ::test-query ::test droid-resolver)
              (leona/attach-field-resolver ::a (constantly {:a 123}))
              (leona/generate))]
    (is (get-in r [:objects :B :fields :a :resolve]))))

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


;;;;;;;

(deftest add-alias-basic-test
  (s/def :foo/status #{:a :b :c})
  (s/def ::my-foo-object (s/keys :req-un [:foo/status]))
  (s/def ::my-query-var int?)
  (s/def ::foo-query-args (s/keys :req-un [::my-query-var]))

  (let [r (-> (leona/create)
              (leona/attach-query ::foo-query-args ::my-foo-object (constantly nil))
              (leona/attach-type-alias :foo/status :foo-status)
              (leona/generate))]
    (is (= '(non-null :FooStatus) (get-in r [:objects :MyFooObject :fields :status :type])))
    (is (= #{:A :B :C} (set (get-in r [:enums :FooStatus :values]))))))

(deftest add-alias-medium-test
  (s/def :foo/status #{:a :b :c})
  (s/def :bar/status #{:d :e :f})
  (s/def ::my-foo-object (s/keys :req-un [:foo/status]))
  (s/def ::my-bar-object (s/keys :req-un [:bar/status]))
  (s/def ::my-query-var int?)
  (s/def ::query-args (s/keys :req-un [::my-query-var]))
  (let [r (-> (leona/create)
              (leona/attach-query ::query-args ::my-foo-object (constantly {:status :a}))
              (leona/attach-query ::query-args ::my-bar-object (constantly {:status :d}))
              (leona/attach-type-alias :foo/status :foo-status)
              (leona/compile))]
    (is (= '(non-null :FooStatus) (get-in r [:generated :objects :MyFooObject :fields :status :type])))
    (is (= '(non-null :Status) (get-in r [:generated :objects :MyBarObject :fields :status :type])))
    (is (= #{:A :B :C} (set (get-in r [:generated :enums :FooStatus :values]))))
    (is (= #{:D :E :F} (set (get-in r [:generated :enums :Status :values]))))
    (let [r (leona/execute r "query { myFooObject(myQueryVar: 1001) { status }}")]
      (is (= :A (get-in r [:data :myFooObject :status]))))))

(deftest add-alias-in-query-test
  (s/def ::value int?)
  (s/def :foo/selector #{:foo :bar})
  (s/def :bar/selector #{:baz :qux})
  (s/def ::my-foo-object (s/keys :req-un [::value :foo/selector]))
  (s/def ::my-bar-object (s/keys :req-un [::value :bar/selector]))
  (s/def ::foo-query-args (s/keys :req-un [:foo/selector]))
  (s/def ::bar-query-args (s/keys :req-un [:bar/selector]))
  (let [r (-> (leona/create)
              (leona/attach-query ::foo-query-args ::my-foo-object (constantly {:value 123 :selector :foo}))
              (leona/attach-query ::bar-query-args ::my-bar-object (constantly {:value 456 :selector :qux}))
              (leona/attach-type-alias :foo/selector :foo-status)
              (leona/compile))]
    (is (= '(non-null :FooStatus) (get-in r [:generated :objects :MyFooObject :fields :selector :type])))
    (is (= '(non-null :FooStatus) (get-in r [:generated :queries :myFooObject :args :selector :type])))
    (is (= #{:FOO :BAR} (set (get-in r [:generated :enums :FooStatus :values]))))
    (is (= #{:BAZ :QUX} (set (get-in r [:generated :enums :Selector :values]))))
    (let [r (leona/execute r "query { myFooObject(selector: FOO) { value }}")]
      (is (= 123 (get-in r [:data :myFooObject :value]))))))

(deftest no-args-query-test
  (s/def ::no-args nil?)
  (s/def ::result string?)
  (s/def ::my-query (s/keys :req-un [::result]))
  (let [my-query-resolver (fn [ctx args value] {:result "hello"})
        schema(-> (leona/create)
                  (leona/attach-query ::no-args ::my-query my-query-resolver)
                  (leona/compile))
        r (leona/execute schema "{ myQuery { result } }")]
    (is (= "hello"  (get-in r [:data :myQuery :result])))))
