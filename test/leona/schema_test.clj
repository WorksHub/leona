(ns leona.schema-test
  (:require  [clojure.spec.alpha :as s]
             [clojure.test :refer :all]
             [leona.lacinia.schema :as schema]
             [leona.test-spec :as test]))

(deftest fix-references-test
  (let [s {:objects {:test {:fields {:b {:objects {:b {:fields {:a {:type '(non-null Int)}}}}},
                                     :d {:objects {:d {:fields {:c {:type '(non-null String)}}}}}}}}}]
    (is (= (schema/fix-references s)
           {:objects {:test {:fields {:b {:type :b},
                                      :d {:type :d}}}
                      :b {:fields {:a {:type '(non-null Int)}}}
                      :d {:fields {:c {:type '(non-null String)}}}}}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest schema-req-test
  (s/def ::a int?)
  (s/def ::test (s/keys :req [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {(schema/clj-name->qualified-gql-name ::a) {:type '(non-null Int)}}}}})))

(deftest schema-req-with-nilable-test
  (s/def ::a (s/nilable int?))
  (s/def ::test (s/keys :req [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {(schema/clj-name->qualified-gql-name ::a) {:type '(non-null Int)}}}}})))

(deftest schema-req-un-test
  (s/def ::a string?)
  (s/def ::test (s/keys :req-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type '(non-null String)}}}}})))

(deftest schema-req-un-with-nilable-test
  (s/def ::a (s/nilable string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type 'String}}}}})))

(deftest schema-opt-test
  (s/def ::a int?)
  (s/def ::test (s/keys :opt [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {(schema/clj-name->qualified-gql-name ::a) {:type 'Int}}}}})))

(deftest schema-opt-with-nilable-test
  (s/def ::a (s/nilable int?))
  (s/def ::test (s/keys :opt [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {(schema/clj-name->qualified-gql-name ::a) {:type 'Int}}}}})))

(deftest schema-opt-un-test
  (s/def ::a string?)
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type 'String}}}}})))

(deftest schema-opt-un-with-nilable-test
  (s/def ::a (s/nilable string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type 'String}}}}})))

(deftest schema-req-list-test
  (s/def ::a (s/coll-of string?))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type '(non-null (list (non-null String)))}}}}})))

(deftest schema-opt-list-test
  (s/def ::a (s/coll-of string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type '(list String)}}}}})))

(deftest schema-req-enum-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::test (s/keys :req-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type '(non-null :a)}}}} :enums {:a {:values [:baz :bar :foo]}}})))

(deftest schema-opt-enum-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type :a}}}} :enums {:a {:values [:baz :bar :foo]}}})))

(deftest schema-req-enum-list-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::b (s/coll-of ::a))
  (s/def ::test (s/keys :req-un [::b]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:b {:type '(non-null (list (non-null :a)))}}}} :enums {:a {:values [:baz :bar :foo]}}})))

(deftest schema-opt-enum-list-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::b (s/coll-of ::a))
  (s/def ::test (s/keys :opt-un [::b]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:b {:type '(list :a)}}}} :enums {:a {:values [:baz :bar :foo]}}})))

(deftest schema-req-un-reference-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c string?)
  (s/def ::d (s/keys :req-un [::c]))
  (s/def ::test (s/keys :req-un [::b ::d]))
  (is (= (schema/transform ::test)
         {:objects {:b {:fields {:a {:type '(non-null Int)}}}
                    :d {:fields {:c {:type '(non-null String)}}}
                    :test {:fields {:b {:type '(non-null :b)},
                                    :d {:type '(non-null :d)}}}}})))

(deftest schema-opt-un-reference-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c string?)
  (s/def ::d (s/keys :opt-un [::c]))
  (s/def ::test (s/keys :opt-un [::b ::d]))
  (is (= (schema/transform ::test)
         {:objects {:b {:fields {:a {:type 'Int}}}
                    :d {:fields {:c {:type 'String}}}
                    :test {:fields {:b {:type :b},
                                    :d {:type :d}}}}})))

(deftest schema-and-test
  "If we recognise a predicate we use that"
  (s/def ::a (s/and int? odd?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:a {:type 'Int}}}}})))

(deftest schema-and-test-fail
  (s/def ::a (s/and even? odd?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (thrown-with-msg? Exception #"Error: 'and' must include a recognised predicate"
                        (schema/transform ::test))))

(deftest schema-exception-test
  (s/def ::a map?)
  (s/def ::test (s/keys :opt-un [::a]))
  (is (thrown-with-msg? Exception #"The following specs could not be transformed: :leona.schema-test/a"
                        (schema/transform ::test))))

(def result
  {:objects
   {:human
    {:fields
     {:home_planet {:type '(non-null String)},
      :id {:type '(non-null Int)},
      :ids {:type '(non-null (list (non-null Int)))},
      :name {:type '(non-null String)},
      :appears_in {:type '(non-null (list (non-null :episode)))},
      :episode {:type :episode}}},
    :droid
    {:fields
     {:primary_functions {:type '(non-null (list (non-null String)))},
      :id {:type '(non-null Int)},
      :name {:type '(non-null String)},
      :appears_in {:type '(non-null (list (non-null :episode)))},
      :ids {:type '(list Int)}}}},
   :enums {:episode {:values [:EMPIRE :NEWHOPE :JEDI]}}})

(deftest comprehensive-schema-test
  (is (= result
         (schema/combine ::test/human
                         ::test/droid))))
