(ns leona.schema-test
  (:require  [clojure.spec.alpha :as s]
             [clojure.test :refer :all]
             [leona.core :as leona]
             [leona.schema :as schema]
             [leona.test-spec :as test]
             [leona.util :as util]
             [spec-tools.core :as st]))

(deftest fix-references-test
  (let [s {:objects {:test {:fields {:b {:objects {:b {:fields {:a {:type '(non-null Int)}}}}},
                                     :d {:objects {:d {:fields {:c {:type '(non-null String)}}}}}}}}}]
    (is (= {:objects {:test {:fields {:b {:type :b},
                                      :d {:type :d}}}
                      :b {:fields {:a {:type '(non-null Int)}}}
                      :d {:fields {:c {:type '(non-null String)}}}}}
           (schema/fix-references s)))))

(deftest valid-enum?-test
  (is (schema/valid-enum? [:foo :bar :baz]))
  (is (schema/valid-enum? ["foo" "bar" "baz"]))
  (is (schema/valid-enum? [:foo_one :bar_two :baz_three]))
  (is (schema/valid-enum? ["foo_one" "bar_two" "baz_three"]))
  (is (schema/valid-enum? ["fooOne" "barTwo" "bazThree"]))
  (is (not (schema/valid-enum? [1 2 3])))
  (is (not (schema/valid-enum? ["foo one" "bar two" "baz three"])))
  (is (not (schema/valid-enum? ["foo-one" "bar-two" "baz-three"])))
  (is (not (schema/valid-enum? [:foo-one :bar-two :baz-three]))))

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
  (is (= {:objects {:Test {:fields {(util/clj-name->qualified-gql-name ::a) {:type '(non-null Int)}}}}}
         (schema/transform ::test))))

(deftest schema-req-with-nilable-test
  (s/def ::a (s/nilable int?))
  (s/def ::test (s/keys :req [::a]))
  (is (= {:objects {:Test {:fields {(util/clj-name->qualified-gql-name ::a) {:type '(non-null Int)}}}}}
         (schema/transform ::test))))

(deftest schema-req-un-test
  (s/def ::a string?)
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type '(non-null String)}}}}}
         (schema/transform ::test))))

(deftest schema-req-un-with-nilable-test
  (s/def ::a (s/nilable string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type 'String}}}}}
         (schema/transform ::test))))

(deftest schema-opt-test
  (s/def ::a int?)
  (s/def ::test (s/keys :opt [::a]))
  (is (= {:objects {:Test {:fields {(util/clj-name->qualified-gql-name ::a) {:type 'Int}}}}}
         (schema/transform ::test))))

(deftest schema-opt-with-nilable-test
  (s/def ::a (s/nilable int?))
  (s/def ::test (s/keys :opt [::a]))
  (is (= {:objects {:Test {:fields {(util/clj-name->qualified-gql-name ::a) {:type 'Int}}}}}
         (schema/transform ::test))))

(deftest schema-opt-un-test
  (s/def ::a string?)
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type 'String}}}}}
         (schema/transform ::test))))

(deftest schema-opt-un-with-nilable-test
  (s/def ::a (s/nilable string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type 'String}}}}}
         (schema/transform ::test))))

(deftest schema-req-list-test
  (s/def ::a (s/coll-of string?))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type '(non-null (list (non-null String)))}}}}}
         (schema/transform ::test))))

(deftest schema-opt-list-test
  (s/def ::a (s/coll-of string?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type '(list String)}}}}}
         (schema/transform ::test))))

(deftest schema-req-enum-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type '(non-null :A)}}}} :enums {:A {:values [:baz :bar :foo]}}}
         (schema/transform ::test))))

(deftest schema-opt-enum-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type :A}}}} :enums {:A {:values [:baz :bar :foo]}}}
         (schema/transform ::test))))

(deftest schema-req-enum-list-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::b (s/coll-of ::a))
  (s/def ::test (s/keys :req-un [::b]))
  (is (= {:objects {:Test {:fields {:B {:type '(non-null (list (non-null :A)))}}}} :enums {:A {:values [:baz :bar :foo]}}}
         (schema/transform ::test))))

(deftest schema-opt-enum-list-test
  (s/def ::a #{:foo :bar :baz})
  (s/def ::b (s/coll-of ::a))
  (s/def ::test (s/keys :opt-un [::b]))
  (is (= {:objects {:Test {:fields {:B {:type '(list :A)}}}} :enums {:A {:values [:baz :bar :foo]}}}
         (schema/transform ::test))))

(deftest schema-req-un-reference-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c string?)
  (s/def ::d (s/keys :req-un [::c]))
  (s/def ::test (s/keys :req-un [::b ::d]))
  (is (= {:objects {:B {:fields {:A {:type '(non-null Int)}}}
                    :D {:fields {:C {:type '(non-null String)}}}
                    :Test {:fields {:B {:type '(non-null :B)},
                                    :D {:type '(non-null :D)}}}}}
         (schema/transform ::test))))

(deftest schema-req-un-reference-opt-un-coll-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c (s/coll-of ::b))
  (s/def ::test (s/keys :opt-un [::c]))
  (is (= {:objects {:B {:fields {:A {:type '(non-null Int)}}}
                    :Test {:fields {:C {:type '(list :B)}}}}}
         (schema/transform ::test))))

(deftest schema-req-un-reference-req-un-coll-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c (s/coll-of ::b))
  (s/def ::test (s/keys :req-un [::c]))
  (is (= {:objects {:B {:fields {:A {:type '(non-null Int)}}}
                    :Test {:fields {:C {:type '(non-null (list (non-null :B)))}}}}}
         (schema/transform ::test))))

(deftest schema-reference-name-req-req-test
  (s/def ::a int?)
  (s/def ::b (s/keys :req-un [::a]))
  (s/def ::c ::b)
  (s/def ::test (s/keys :req-un [::c]))
  (is (= {:objects {:B {:fields {:A {:type '(non-null Int)}}}
                    :Test {:fields {:C {:type '(non-null :B)},}}}}
         (schema/transform ::test))))

(deftest schema-reference-name-opt-req-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c ::b)
  (s/def ::test (s/keys :req-un [::c]))
  (is (= {:objects {:B {:fields {:A {:type 'Int}}}
                    :Test {:fields {:C {:type '(non-null :B)},}}}}
         (schema/transform ::test))))

(deftest schema-reference-name-opt-opt-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c ::b)
  (s/def ::test (s/keys :opt-un [::c]))
  (is (= {:objects {:B {:fields {:A {:type 'Int}}}
                    :Test {:fields {:C {:type :B},}}}}
         (schema/transform ::test))))

(deftest schema-opt-un-reference-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c string?)
  (s/def ::d (s/keys :opt-un [::c]))
  (s/def ::test (s/keys :opt-un [::b ::d]))
  (is (= {:objects {:B {:fields {:A {:type 'Int}}}
                    :D {:fields {:C {:type 'String}}}
                    :Test {:fields {:B {:type :B},
                                    :D {:type :D}}}}}
         (schema/transform ::test))))

(deftest schema-merge-test
  (s/def ::a int?)
  (s/def ::b (s/keys :opt-un [::a]))
  (s/def ::c string?)
  (s/def ::d (s/keys :opt-un [::c]))
  (s/def ::test (s/merge ::b ::d))
  (is (= {:objects {:Test {:fields {:A {:type 'Int},
                                    :C {:type 'String}}}}}
         (schema/transform ::test))))

(deftest schema-and-test
  "If we recognise a predicate we use that"
  (s/def ::a (s/and int? odd?))
  (s/def ::test (s/keys :opt-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type 'Int}}}}}
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
  (is (= {:objects {:Test {:fields {:A {:type 'Int}}}}}
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
     {:HomePlanet {:type '(non-null String)},
      :Id {:type '(non-null Int)},
      :Name {:type '(non-null String)},
      :AppearsIn {:type '(non-null (list (non-null :Episode)))},
      :Episode {:type :Episode}}},
    :Droid
    {:fields
     {:PrimaryFunctions {:type '(non-null (list (non-null String)))},
      :Id {:type '(non-null Int)},
      :Name {:type '(non-null String)},
      :Owner      {:type :Human},
      (util/clj-name->qualified-gql-name ::test/appears-in) {:type '(non-null (list (non-null :Episode)))},
      :Operational_QMARK_ {:type 'Boolean}}}},
   :enums {:Episode {:values [:EMPIRE :NEWHOPE :JEDI]}}})

(deftest comprehensive-schema-test
  (is (= result
         (schema/combine ::test/human
                         ::test/droid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest schema-description-test
  (s/def ::a (st/spec int? {:description "FooBarBaz"}))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type '(non-null Int) :description "FooBarBaz"}}}}}
         (schema/transform ::test))))

(deftest schema-type-test
  (s/def ::a (st/spec int? {:type 'Boolean}))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type 'Boolean}}}}}
         (schema/transform ::test))))

(deftest schema-type-enum-test
  (s/def ::a (st/spec int? {:type '(enum :foo)}))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type :foo}}}}}
         (schema/transform ::test))))

(deftest schema-type-kw-ignored-test
  (s/def ::a (st/spec int? {:type :foo}))
  (s/def ::test (s/keys :req-un [::a]))
  (is (= {:objects {:Test {:fields {:A {:type '(non-null Int)}}}}}
         (schema/transform ::test))))
