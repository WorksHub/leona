(ns leona.test-spec
  (:require  [clojure.spec.alpha :as s]
             [clojure.test :as t]))

(s/def ::primary-functions (s/coll-of string?))
(s/def ::home-planet string?)
(s/def ::id int?)
(s/def ::ids (s/coll-of ::id))
(s/def ::name string?)
(s/def ::episode #{:NEWHOPE :EMPIRE :JEDI})
(s/def ::appears-in (s/coll-of ::episode))

(s/def ::droid (s/keys :req-un [::primary-functions
                                ::id
                                ::name
                                ::appears-in]
                       :opt-un [::ids]))

(s/def ::human (s/keys
               :req-un [::home-planet
                        ::id
                        ::ids
                        ::name
                        ::appears-in]
               :opt-un [::episode]))
