(ns leona.real-schema-test
  (:require  [clojure.spec.alpha :as s]
             [clojure.spec.gen.alpha :as gen]
             [clojure.string :as str]
             [clojure.test :refer :all]
             [leona.core :as leona]
             [leona.schema :as schema]))

;; Location
(s/def :wh.location/street string?)
(s/def :wh.location/post-code string?)
(s/def :wh.location/city string?)
(s/def :wh.location/state string?)
(s/def :wh.location/administrative string?)
(s/def :wh.location/country string?)
(s/def :wh.location/sub-region (s/nilable string?))
(s/def :wh.location/region (s/nilable string?))
(s/def :wh.location/latitude (s/or :double (s/double-in :min -90.0 :max 90 :NaN false :infinite? false)
                                   :int (s/int-in -90 90)))
(s/def :wh.location/longitude (s/or :double (s/double-in :min -180.0 :max 180 :NaN false :infinite? false)
                                    :int (s/int-in -180 180)))
(s/def :wh.location/timezone string?)

;; Company

(defn valid-url-characters? [s]
  (boolean (re-find (re-pattern "^[A-Za-z0-9._~()'!*:@,;+?–-—-]*(\\$)*\\[*\\]*#*=*&*/*$") s)))

(s/fdef valid-url-characters?
  :args (s/cat :s string?)
  :ret boolean?)

(defn valid-m5d-string? [s]
  (boolean (re-find #"^[a-fA-F0-9]+$" s)))

(s/fdef valid-m5d-string?
  :args (s/cat :s string?)
  :ret boolean?)

(defn valid-slug? [s]
  (let [no-spaces? (not (boolean (re-find #"\s+" s)))
        url-valid-characters (valid-url-characters? s)
        md5-hash-characters? (valid-m5d-string? (apply str (take-last 5 s)))]
    (and url-valid-characters
         no-spaces?
         md5-hash-characters?)))

(defn slugify [s] s)

(s/def :wh.company/id (s/with-gen
                        (s/and string? valid-slug?)
                        #(gen/fmap
                          (fn [s] (slugify s))
                          (gen/not-empty (s/gen string?)))))

(s/def ::not-blank-string (s/and string? (complement str/blank?)))

(s/def :wh.company/name ::not-blank-string)
(s/def :wh.company/logo string?)
(s/def :wh.company/description string?)
(s/def :wh/company (s/keys :req-un [:wh.company/name
                                    :wh.company/id]
                           :opt-un [:wh.company/logo
                                    :wh.company/description]))

;; Salary

(s/def :wh.salary/max int?)
(s/def :wh.salary/min int?)
(s/def :wh.salary/currency string?)
(s/def :wh/salary (s/keys :req-un [:wh.salary/max
                                   :wh.salary/min
                                   :wh.salary/currency]))

;; Job

(s/def :wh.job/id (s/with-gen
                    (s/and string? valid-slug?)
                    #(gen/fmap
                      (fn [s] (slugify s))
                      (gen/not-empty (s/gen string?)))))

(s/def :wh.job/title ::not-blank-string)
(s/def :wh.job/description string?)
(s/def :wh.job/tagline string?)
(s/def :wh.job/benefit string?)
(s/def :wh.job/benefits (s/coll-of :wh.job/benefit))
(s/def :wh.job/tags (s/coll-of string?))
(s/def :wh.job/remote? boolean?)
(s/def :wh.job/sponsorship-offered? boolean?)
(s/def :wh.job/published? boolean?)
(s/def :wh.job/promoted? boolean?)
(s/def :wh.job/creation-date inst?)
(s/def :wh.job/role-type #{"Full_time" "Contract" "Intern"})
(s/def :wh.job/company :wh/company)
(s/def :wh.job/salary :wh/salary)
(s/def :wh.job/location (s/keys :req-un [:wh.location/country]
                                :opt-un [:wh.location/street
                                         :wh.location/city
                                         :wh.location/state
                                         :wh.location/longitude
                                         :wh.location/latitude
                                         :wh.location/post-code
                                         :wh.location/sub-region
                                         :wh.location/region]))

(s/def :wh/job (s/keys :req-un [:wh.job/id
                                :wh.job/title
                                :wh.job/description
                                :wh.job/tags
                                :wh.job/remote?
                                :wh.job/sponsorship-offered?
                                :wh.job/published?
                                :wh.job/role-type
                                :wh.job/company
                                :wh.job/creation-date
                                :wh.job/location]
                       :opt-un [:wh.job/benefits
                                :wh.job/tagline
                                :wh.job/salary]))

(deftest real-schema-test
  (let [r (schema/transform :wh/job)]
    (is r)
    (is (= #{:Location :Company :Salary :Job} (set (keys (:objects r)))))
    (is (= {:RoleType {:values ["Intern" "Contract" "Full_time"]}} (:enums r)))))

(deftest real-compile
  (s/def ::job-input (s/keys :req-un [:wh.job/id]))
  (let [job-resolver (fn [c q v])
        compiled-schema (-> (leona/create)
                            (leona/attach-query ::job-input :wh/job job-resolver)
                            (leona/compile))]
    (is (:compiled compiled-schema))))
