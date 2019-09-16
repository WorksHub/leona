(ns leona.util-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [leona.util :as util]))

(deftest replace-punctuation-test
  (is (= "hello_QMARK_" (util/replace-punctuation "hello?")))
  (is (= "hello_QMARK__QMARK_" (util/replace-punctuation "hello??")))
  (is (= "hello_XMARK_" (util/replace-punctuation "hello!"))))

(deftest replace-placeholders-test
  (is (= "hello?" (util/replace-placeholders "hello_QMARK_")))
  (is (= "hello??" (util/replace-placeholders "hello_QMARK__QMARK_")))
  (is (= "hello!" (util/replace-placeholders "hello_XMARK_"))))

(deftest clj-name->gql-name-test
  (is (= :hello (util/clj-name->gql-name :hello)))
  (is (= :hello (util/clj-name->gql-name ::hello)))
  (is (= :hello_QMARK_ (util/clj-name->gql-name ::hello?)))
  (is (= :hello_QMARK_ (util/clj-name->gql-name :leona.util-test/hello?))))

(deftest clj-name->qualified-gql-name-test
  (is (= :hello (util/clj-name->qualified-gql-name :hello)))
  (is (= :leona__util_test___hello (util/clj-name->qualified-gql-name ::hello)))
  (is (= :leona__util_test___hello_QMARK_ (util/clj-name->qualified-gql-name ::hello?)))
  (is (= :leona__util_test___hello_QMARK_ (util/clj-name->qualified-gql-name :leona.util-test/hello?))))

(deftest clj-name->qualified-gql-name-test
  (is (= :hello (util/gql-name->clj-name :hello)))
  (is (= :leona.util-test/hello (util/gql-name->clj-name :leona__util_test___hello)))
  (is (= ::hello? (util/gql-name->clj-name :leona__util_test___hello_QMARK_)))
  (is (= :leona.util-test/hello? (util/gql-name->clj-name :leona__util_test___hello_QMARK_))))

(deftest spec-keys-test
  (s/def ::bar int?)
  (s/def ::baz string?)
  (s/def ::foo (s/keys :req-un [::bar]
                       :opt-un [::baz]))
  (is (= '(::bar ::baz) (util/spec-keys ::foo)))
  (is (nil? (util/spec-keys int?)))
  (is (nil? (util/spec-keys nil?))))

(deftest find-case-match
  (is (= :foo (util/find-case-match {:foo 123} :foo)))
  (comment (is (= :fooBar (util/find-case-match {:fooBar 123} :foo_bar)))
           (is (= :FooBar (util/find-case-match {:FooBar 123} :foo_bar)))
           (is (= :FOO_BAR (util/find-case-match {:FOO_BAR 123} :foo_bar)))
           (is (= :foo_bar (util/find-case-match {:foo_bar 123} :FooBar)))
           (is (= :foo_bar (util/find-case-match {:foo_bar 123} :fooBar)))
           (is (= :foo_bar (util/find-case-match {:foo_bar 123} :FOO_BAR)))))
