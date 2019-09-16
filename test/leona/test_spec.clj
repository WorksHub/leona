(ns leona.test-spec
  (:require  [clojure.spec.alpha :as s]
             [clojure.test :as t]))

(s/def ::primary-functions (s/coll-of string?))
(s/def ::home-planet string?)
(s/def ::id (s/and int? odd?))
(s/def ::name string?)
(s/def ::operational? boolean?)
(s/def ::episode #{:new-hope :empire :jedi})
(s/def ::appears-in (s/coll-of ::episode))

(s/def ::human (s/keys
                :req-un [::home-planet
                         ::id
                         ::name
                         ::appears-in]
                :opt-un [::episode]))

(s/def ::owner ::human)

(s/def ::droid (s/keys :req-un [::primary-functions
                                ::id
                                ::name]
                       :req [::appears-in]
                       :opt-un [::operational?
                                ::owner]))

(s/def ::droid-query (s/keys :req-un [::id]
                             :opt [::appears-in]))

(s/def ::droid-mutation (s/keys :req-un [::id]
                                :opt-un [::primary-functions]))

(s/def ::test-query-enum #{:a :b :c})
(s/def ::another-droid-query (s/keys :req-un [::id
                                              ::test-query-enum]
                                     :opt [::appears-in]))
