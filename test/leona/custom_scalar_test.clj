(ns leona.custom-scalar-test
  (:require  [clj-time.core :as t]
             [clj-time.format :as tf]
             [clj-time.types :as tt]
             [clojure.spec.alpha :as s]
             [clojure.test :refer :all]
             [leona.core :as leona]
             [spec-tools.core :as st]))

(deftest custom-scalar-test
  (s/def ::date (st/spec tt/date-time? {:type 'String}))
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
    (is (= :date (get-in compiled-schema [:generated :queries :test :args :date :type])))
    (is (= :date (get-in compiled-schema [:generated :objects :result :fields :date :type])))
    (let [d (get-in result [:data :test :result :date])]
      (is (= (str (t/plus now-date (t/years 1))) d)))))

;; This kind of post-generation replacement will not work for places where the spec is exploded
;; Basically, unless we are replacing an object, we've lost of the spec by the time we get to this stage.
;; What we should do instead:
;; Schema generation has custom scalars info passed in and when we encounter such a spec that is flagged as a CS
;; you literally leave it alone and do not replace it with the form. We don't even want to give it a special notation lest
;; we fuck with people's queries like we have with input objects.

#_(deftest custom-scalar-test--collection
    (s/def ::date (st/spec tt/date-time? {:type 'String}))
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
      (is (= :date (get-in compiled-schema [:generated :queries :test :args :date :type])))
      (is (= :date (get-in compiled-schema [:generated :objects :result :fields :date :type])))
      (let [d (get-in result [:data :test :result :dates])]
        (is (= (str (t/plus now-date (t/years 1))) (first d)))
        (is (= (str (t/minus now-date (t/years 1))) (second d))))))
