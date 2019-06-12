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
  (is (= {:droid
          {:type :droid,
           :args {:id {:type '(non-null Int)},
                  (util/clj-name->qualified-gql-name ::test/appears-in) {:type '(list :episode)}}}}
         (-> (leona/create)
             (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
             (leona/generate)
             :queries
             (update :droid dissoc :resolve))))) ;; remove resolver because it gets wrapped

(deftest generate-mutation-test
  (is (= {:droid
          {:type :droid,
           :args {:id {:type '(non-null Int)},
                  :primary_functions {:type '(list String)}}}}
         (-> (leona/create)
             (leona/attach-mutation ::test/droid-mutation ::test/droid droid-mutator)
             (leona/generate)
             :mutations
             (update :droid dissoc :resolve)))))

(deftest attach-object-test
  (is (= (-> (leona/create)
             (leona/attach-object ::test/human)
             (leona/generate)
             (get-in [:objects :human :fields]))
         '{:home_planet {:type (non-null String)},
           :id {:type (non-null Int)},
           :name {:type (non-null String)},
           :appears_in {:type (non-null (list (non-null :episode)))},
           :episode {:type :episode}})))

(deftest query-valid-test
  (let [appears-in-str (name (util/clj-name->qualified-gql-name ::test/appears-in))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test/droid-query ::test/droid droid-resolver)
                            (leona/compile))
        result (leona/execute compiled-schema
                              (format "query { droid(id: 1001, %s: NEWHOPE) { name, operational_QMARK_, %s }}"
                                      appears-in-str
                                      appears-in-str))]
    (is (= "R2D2" (get-in result [:data :droid :name])))
    (is (= true (get-in result [:data :droid :operational_QMARK_])))
    (is (= '(:NEWHOPE :EMPIRE :JEDI) (get-in result [:data :droid (keyword appears-in-str)])))))

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
                              "mutation { droid(id: 1001, primary_functions: [\"beep\"]) { name, operational_QMARK_, primary_functions }}")]
    (is (= "R2D2"   (get-in result [:data :droid :name])))
    (is (= true     (get-in result [:data :droid :operational_QMARK_])))
    (is (= ["beep"] (get-in result [:data :droid :primary_functions])))))

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
        result (leona/execute compiled-schema "query Test($input: input_input!) { test(input: $input) { num }}" {:input {:num 1, :nums [2 3]}} {})]
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
        result (leona/execute compiled-schema "query Test($test_query: test_query_input!) { test(test_query: $test_query) { num }}"
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
        result (leona/execute compiled-schema "query Test($inputs: [input_input!]!) { test(inputs: $inputs) { num }}" {:inputs [{:num 1, :nums [2 3]}]} {})]
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
               :appears-in #{:JEDI}}
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
    (is (get-in r [:objects :test :fields :b :resolve]))))

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
    (is (get-in r [:objects :test :fields :c :resolve]))))

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
    (is (get-in r [:objects :b :fields :a :resolve]))))

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
    (is (= '(non-null :foo_status) (get-in r [:objects :my_foo_object :fields :status :type])))
    (is (= #{:a :b :c} (set (get-in r [:enums :foo_status :values]))))))

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
    (is (= '(non-null :foo_status) (get-in r [:generated :objects :my_foo_object :fields :status :type])))
    (is (= '(non-null :status) (get-in r [:generated :objects :my_bar_object :fields :status :type])))
    (is (= #{:a :b :c} (set (get-in r [:generated :enums :foo_status :values]))))
    (is (= #{:d :e :f} (set (get-in r [:generated :enums :status :values]))))
    (let [r (leona/execute r "query { my_foo_object(my_query_var: 1001) { status }}")]
      (is (= :a (get-in r [:data :my_foo_object :status]))))))

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
    (is (= '(non-null :foo_status) (get-in r [:generated :objects :my_foo_object :fields :selector :type])))
    (is (= '(non-null :foo_status) (get-in r [:generated :queries :my_foo_object :args :selector :type])))
    (is (= #{:foo :bar} (set (get-in r [:generated :enums :foo_status :values]))))
    (is (= #{:baz :qux} (set (get-in r [:generated :enums :selector :values]))))
    (let [r (leona/execute r "query { my_foo_object(selector: foo) { value }}")]
      (is (= 123 (get-in r [:data :my_foo_object :value]))))))
