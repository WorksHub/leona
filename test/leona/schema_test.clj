(ns leona.schema-test
  (:require  [clojure.spec.alpha :as s]
             [clojure.test :refer :all]
             [leona.core :as leona]
             [leona.schema :as schema]
             [leona.test-spec :as test]
             [leona.util :as util]
             [spec-tools.core :as st]))

(deftest fix-references-test
  (let [s {:objects {:Test {:fields {:b {:objects {:B {:fields {:a {:type '(non-null Int)}}}}},
                                     :d {:objects {:D {:fields {:c {:type '(non-null String)}}}}}}}}}]
    (is (= {:objects {:Test {:fields {:b {:type :B},
                                      :d {:type :D}}}
                      :B    {:fields {:a {:type '(non-null Int)}}}
                      :D    {:fields {:c {:type '(non-null String)}}}}}
           (schema/fix-references s {})))))

(deftest valid-replacement-type?-test
  (is (schema/valid-replacement-type? 'String))
  (is (schema/valid-replacement-type? 'Float))
  (is (schema/valid-replacement-type? 'Int))
  (is (schema/valid-replacement-type? 'Boolean))
  (is (schema/valid-replacement-type? 'ID))
  (is (schema/valid-replacement-type? '(list Int)))
  (is (schema/valid-replacement-type? '(non-null Float)))
  (is (schema/valid-replacement-type? '(enum :Int)))
  (is (not (schema/valid-replacement-type? :Int)))
  (is (not (schema/valid-replacement-type? nil)))
  (is (not (schema/valid-replacement-type? "Foo")))
  (is (not (schema/valid-replacement-type? int?)))
  (is (not (schema/valid-replacement-type? [double?]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest schema-req-test
  (s/def ::a int?)
  (s/def ::test (s/keys :req [::a]))
  (is (= {:objects {:Test {:fields {(util/clj-name->qualified-gql-name ::a)
                                    {:type '(non-null Int)
                                     :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-req-with-nilable-test
  (s/def ::a (s/nilable int?))
  (s/def ::test (s/keys :req [::a]))
  (is (= {:objects {:Test {:fields {(util/clj-name->qualified-gql-name ::a)
                                    {:type '(non-null Int)
                                     :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-req-un-test
  (s/def ::a string?)
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type '(non-null String)
                                        :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-req-un-with-nilable-test
  (s/def ::a (s/nilable string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type 'String
                                        :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-opt-test
  (s/def ::a int?)
  (s/def ::test (s/keys :opt [::a]))
  (is (= {:objects {:Test {:fields {(util/clj-name->qualified-gql-name ::a)
                                    {:type 'Int
                                     :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-opt-with-nilable-test
  (s/def ::a (s/nilable int?))
  (s/def ::test (s/keys :opt [::a]))
  (is (= {:objects {:Test {:fields {(util/clj-name->qualified-gql-name ::a)
                                    {:type 'Int
                                     :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-opt-un-test
  (s/def ::a string?)
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type 'String
                                        :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-opt-un-with-nilable-test
  (s/def ::a (s/nilable string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type 'String
                                        :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-req-list-test
  (s/def ::a (s/coll-of string?))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type '(non-null (list (non-null String)))
                                        :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-opt-list-test
  (s/def ::a (s/coll-of string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type '(list String)
                                        :spec ::a}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-req-enum-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type '(non-null :A)
                                        :spec ::a}}
                           :spec ::test}}
          :enums {:A {:values [:BAZ :BAR :FOO]}}}
         (schema/transform ::test))))

(deftest schema-opt-enum-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type :A
                                        :spec ::a}}
                           :spec ::test}}
          :enums {:A {:values [:BAZ :BAR :FOO]}}}
         (schema/transform ::test))))

(deftest schema-req-enum-list-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::b (s/coll-of ::a))
  (s/def ::test (s/keys :req-un [::b]))
  (is (= {:objects {:Test {:fields {:b {:type '(non-null (list (non-null :A)))
                                        :spec ::b}}
                           :spec ::test}}
          :enums {:A {:values [:BAZ :BAR :FOO]}}}
         (schema/transform ::test))))

(deftest schema-opt-enum-list-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::b (s/coll-of ::a))
  (s/def ::test (s/keys :opt-un [::b]))
  (is (= {:objects {:Test {:fields {:b {:type '(list :A)
                                        :spec ::b}}
                           :spec ::test}}
          :enums {:A {:values [:BAZ :BAR :FOO]}}}
         (schema/transform ::test))))

(deftest schema-enum-symbol-test
  (def my-set #{:foo :bar :baz})
  (s/def ::a my-set)
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type '(non-null :A)
                                        :spec ::a}}
                           :spec ::test}}
          :enums {:A {:values [:BAZ :BAR :FOO]}}}
         (schema/transform ::test))))

(deftest schema-req-un-reference-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c string?)
  (s/def ::d (s/keys :req-un [::c]))
  (s/def ::test (s/keys :req-un [::b ::d]))
  (is (= {:objects {:B    {:fields {:a {:type '(non-null Int)
                                        :spec ::a}}
                           :spec ::b}
                    :D    {:fields {:c {:type '(non-null String)
                                        :spec ::c}}
                           :spec ::d}
                    :Test {:fields {:b {:type '(non-null :B)
                                        :spec ::b},
                                    :d {:type '(non-null :D)
                                        :spec ::d}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-req-un-reference-opt-un-coll-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c (s/coll-of ::b))
  (s/def ::test (s/keys :opt-un [::c]))
  (is (= {:objects {:B    {:fields {:a {:type '(non-null Int)
                                        :spec ::a}}
                           :spec ::b}
                    :Test {:fields {:c {:type '(list :B)
                                        :spec ::c}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-req-un-reference-req-un-coll-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c (s/coll-of ::b))
  (s/def ::test (s/keys :req-un [::c]))
  (is (= {:objects {:B    {:fields {:a {:type '(non-null Int)
                                        :spec ::a}}
                           :spec ::b}
                    :Test {:fields {:c {:type '(non-null (list (non-null :B)))
                                        :spec ::c}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-reference-name-req-req-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c ::b)
  (s/def ::test (s/keys :req-un [::c]))
  (is (= {:objects {:B    {:fields {:a {:type '(non-null Int)
                                        :spec ::a}}
                           :spec ::b}
                    :Test {:fields {:c {:type '(non-null :B)
                                        :spec ::c}}
                           :spec ::test}}}
         (schema/transform ::test))))

(deftest schema-reference-name-opt-req-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c ::b)
  (s/def ::test (s/keys :req-un [::c]))
  (is (= {:objects {:B    {:fields {:a {:type 'Int
                                        :spec ::a}}
                           :spec ::b}
                    :Test {:fields {:c {:type '(non-null :B)
                                        :spec ::c}}
                           :spec ::test}}}
         (schema/transform ::test))))

;; <<<<<<<<<<<<<<<<<<<<<< BELOW

(deftest schema-reference-name-opt-opt-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c ::b)
  (s/def ::test (s/keys :opt-un [::c]))
  (is (= {:objects {:B    {:fields {:a {:type 'Int}}}
                    :Test {:fields {:c {:type :B},}}}}
         (schema/transform ::test))))

(deftest schema-opt-un-reference-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c string?)
  (s/def ::d (s/keys :opt-un [::c]))
  (s/def ::test (s/keys :opt-un [::b ::d]))
  (is (= {:objects {:B    {:fields {:a {:type 'Int}}}
                    :D    {:fields {:c {:type 'String}}}
                    :Test {:fields {:b {:type :B},
                                    :d {:type :D}}}}}
         (schema/transform ::test))))

(deftest schema-merge-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c string?)
  (s/def ::d (s/keys :opt-un [::c]))
  (s/def ::test (s/merge ::b ::d))
  (is (= {:objects {:Test {:fields {:a {:type 'Int},
                                    :c {:type 'String}}}}}
         (schema/transform ::test))))

(deftest schema-and-test
  "If we recognise a predicate we use that"
  (s/def ::a (s/and int? odd?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type 'Int}}}}}
         (schema/transform ::test))))

(deftest schema-and-test-fail
  (s/def ::a (s/and even? odd?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (thrown-with-msg? Exception #"Error: 's/and' must include a recognised predicate"
                        (schema/transform ::test))))

(deftest schema-or-test
  "If we recognise a predicate we use that"
  (s/def ::a (s/or :int int? :odd odd?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type 'Int}}}}}
         (schema/transform ::test))))

(deftest schema-or-test-fail
  (s/def ::a (s/or :even even? :odd odd?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (thrown-with-msg? Exception #"Error: 's/or' must include a recognised predicate"
                        (schema/transform ::test))))


(deftest schema-exception-test
  (s/def ::a map?)
  (s/def ::test (s/keys :opt-un [::a]))
  (is (thrown-with-msg? Exception #"Spec could not be transformed"
                        (schema/transform ::test))))

(deftest find-invalid-key-test
  (is (schema/find-invalid-key {:a :leona.schema/invalid}))
  (is (schema/find-invalid-key {:a {:b :leona.schema/invalid}}))
  (is (schema/find-invalid-key {:a {:b {:c 1 :d :leona.schema/invalid}}}))
  (is (not (schema/find-invalid-key {:a 1})))
  (is (not (schema/find-invalid-key {:a [:leona.schema/invalid]})))
  (is (not (schema/find-invalid-key {:leona.schema/invalid 1}))))

(def result
  {:objects
   {:Human
    {:fields
     {:homePlanet {:type '(non-null String)},
      :id         {:type '(non-null Int)},
      :name       {:type '(non-null String)},
      :appearsIn  {:type '(non-null (list (non-null :Episode)))},
      :episode    {:type :Episode}}},
    :Droid
    {:fields
     {:primaryFunctions                                     {:type '(non-null (list (non-null String)))},
      :id                                                   {:type '(non-null Int)},
      :name                                                 {:type '(non-null String)},
      :owner                                                {:type :Human},
      (util/clj-name->qualified-gql-name ::test/appears-in) {:type '(non-null (list (non-null :Episode)))},
      :operational_QMARK_                                   {:type 'Boolean}}}},
   :enums {:Episode {:values [:JEDI :NEW_HOPE :EMPIRE]}}})

(deftest comprehensive-schema-test
  (is (= result
         (schema/combine ::test/human
                         ::test/droid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest schema-description-test
  (s/def ::a (st/spec int? {:description "FooBarBaz"}))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type '(non-null Int) :description "FooBarBaz"}}}}}
         (schema/transform ::test))))

(deftest schema-type-test-req-un
  (s/def ::a (st/spec int? {:type '(non-null Boolean)}))
  (s/def ::b (st/spec int? {:type 'Boolean}))
  (s/def ::test (s/keys :req-un [::a ::b]))
  (is (= {:objects {:Test {:fields {:a {:type '(non-null Boolean)}
                                    ;; TODO ::b should be non-null as field is required.
                                    ;; Workaround is always make field overrides use non-null
                                    :b {:type 'Boolean}}}}}
         (schema/transform ::test))))

(deftest schema-type-test-opt-un
  (s/def ::a (st/spec int? {:type '(non-null Boolean)}))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type 'Boolean}}}}} ;; notice non-null is removed due to opt-un
         (schema/transform ::test))))

(deftest schema-type-enum-test
  (s/def ::a (st/spec int? {:type '(enum :foo)}))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type :foo}}}}}
         (schema/transform ::test))))

(deftest schema-type-kw-ignored-test
  (s/def ::a (st/spec int? {:type :foo}))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:a {:type '(non-null Int)}}}}}
         (schema/transform ::test))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest type-alias-enum-test
  (s/def :foo/status #{:a :b :c})
  (s/def ::test (s/keys :req-un [:foo/status]))
  (is (= {:objects {:Test {:fields {:status {:type '(non-null :foo_status)}}}}
          :enums   {:foo_status {:values [:C :B :A]}}}
         (schema/transform ::test {:type-aliases {:foo/status :foo_status}}))))

(deftest type-alias-object-test
  (s/def ::foo int?)
  (s/def ::bar (s/keys :opt-un [::foo]))
  (s/def ::test (s/keys :req-un [::bar]))
  (is (= {:objects {:Test {:fields {:bar {:type '(non-null :baz)}}}
                    :baz  {:fields {:foo {:type 'Int}}}}}
         (schema/transform ::test {:type-aliases {::bar :baz}}))))
