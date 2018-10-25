(ns leona.schema-test
  (:require  [clojure.spec.alpha :as s]
             [clojure.test :refer :all]
             [leona.lacinia.schema :as schema]))

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
         {:objects {:test {:fields {:a {:type '(non-null String)}}}}})))

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

(deftest schema-exception-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::b map?)
  (s/def ::test (s/keys :opt-un [::b]))
  (is (= (schema/transform ::test)
         {:objects {:test {:fields {:b {:type '(list :a)}}}} :enums {:a {:values [:baz :bar :foo]}}})))
