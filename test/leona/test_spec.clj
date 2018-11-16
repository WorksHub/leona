(ns leona.test-spec
  (:require  [clojure.spec.alpha :as s]
             [clojure.test :as t]))

(s/def ::primary-functions (s/coll-of string?))
(s/def ::home-planet string?)
(s/def ::id (s/and int? odd?))
(s/def ::name string?)
(s/def ::operational? boolean?)
(s/def ::episode #{:NEWHOPE :EMPIRE :JEDI})
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
                       :opt [::owner]
                       :opt-un [::operational?]))

(s/def ::droid-query (s/keys :req-un [::id]
                             :opt [::appears-in]))

(s/def ::droid-mutation (s/keys :req-un [::id]
                                :opt-un [::primary-functions]))
