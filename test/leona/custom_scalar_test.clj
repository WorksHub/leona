(ns leona.custom-scalar-test
  (:require  [clj-time.core :as t]
             [clj-time.format :as tf]
             [clj-time.types :as tt]
             [clojure.spec.alpha :as s]
             [clojure.string :as str]
             [clojure.test :refer :all]
             [leona.core :as leona]
             [leona.schema :as leona-schema]
             [spec-tools.core :as st]))

(deftest custom-scalars-are-preserved-at-schema-time
  (s/def ::date tt/date-time?)
  (s/def ::num int?)
  (s/def ::bool boolean?)
  (s/def ::result-1 (s/keys :req-un [::date]))
  (s/def ::result-2 (s/keys :opt-un [::num]))
  (s/def ::result-3 (s/keys :req-un [::bool]))
  (let [r (leona-schema/combine-with-opts
            {:custom-scalars {::date {:parse identity :serialize identity}
                              ::num  {:parse identity :serialize identity}}}
            ::result-1
            ::result-2
            ::result-3)]
    (is (= r {:objects {:result_1 {:fields {:date {:type '(non-null :date)}}}, :result_2 {:fields {:num {:type :num}}}, :result_3 {:fields {:bool {:type '(non-null Boolean)}}}}}))))

(deftest custom-scalar-test
  (s/def ::date tt/date-time?)
  (s/def ::result (s/keys :req-un [::date]))
  (s/def ::test (s/keys :req-un [::result]))
  (s/def ::test-query (s/keys :req-un [::date]))
  (let [now-date (t/now)
        resolver (fn [ctx query value]
                   (let [{:keys [date]} query]
                     {:result {:date (t/plus date (t/years 1))}}))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test-query ::test resolver)
                            (leona/attach-custom-scalar ::date {:parse #(tf/parse (tf/formatters :date-time) %)
                                                                :serialize #(tf/unparse (tf/formatters :date-time) %)})
                            (leona/compile))
        result (leona/execute compiled-schema "query Test($date: date!) { test(date: $date) { result {date} }}" {:date (str now-date)} {})]
    (is (= '(non-null :date) (get-in compiled-schema [:generated :queries :test :args :date :type])))
    (is (= '(non-null :date) (get-in compiled-schema [:generated :objects :result :fields :date :type])))
    (let [d (get-in result [:data :test :result :date])]
      (is (= (str (t/plus now-date (t/years 1))) d)))))

(deftest custom-scalar-test--collection
  (s/def ::date tt/date-time?)
  (s/def ::dates (s/coll-of ::date))
  (s/def ::result (s/keys :req-un [::dates]))
  (s/def ::test (s/keys :req-un [::result]))
  (s/def ::test-query (s/keys :req-un [::date]))
  (let [now-date (t/now)
        resolver (fn [ctx query value]
                   (let [{:keys [date]} query]
                     {:result {:dates [(t/plus date (t/years 1))
                                       (t/minus date (t/years 1))]}}))
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::test-query ::test resolver)
                            (leona/attach-custom-scalar ::date {:parse #(tf/parse (tf/formatters :date-time) %)
                                                                :serialize #(tf/unparse (tf/formatters :date-time) %)})
                            (leona/compile))
        result (leona/execute compiled-schema "query Test($date: date!) { test(date: $date) { result {dates} }}" {:date (str now-date)} {})]
    (is (= '(non-null :date) (get-in compiled-schema [:generated :queries :test :args :date :type])))
    (is (= '(non-null (list (non-null :date))) (get-in compiled-schema [:generated :objects :result :fields :dates :type])))
    (let [d (get-in result [:data :test :result :dates])]
      (is (= (str (t/plus now-date (t/years 1))) (first d)))
      (is (= (str (t/minus now-date (t/years 1))) (second d))))))
