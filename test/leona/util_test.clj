(ns leona.util-test
  (:require [clojure.test :refer :all]
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
