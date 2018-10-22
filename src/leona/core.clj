(ns leona.core
  (:require [clojure.spec.alpha :as s]
            [com.walmartlabs.lacinia.schema :as schema]
            [leona.lacinia.schema :refer [transform]]))


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
                                ::appears-in]))

(s/def ::human (s/keys :req-un [::home-planet
                                ::id
                                ::ids
                                ::name
                                ::episode
                                ::appears-in]))

(defn create-schema! [] {:query-specs #{}})

(defn attach-query!
  [spec query])

(defn generate-schema!
  [])

#_(-> (create-schema!)
      (attach-query! ::droid
                     (fn [ctx query value])))

#_(generate-schema!)

"
{:enums
 {:episode
  {:description \"The episodes of the original Star Wars trilogy.\"
:values [:NEWHOPE :EMPIRE :JEDI]}}

 :objects
 {:droid
  {:fields {:primary_functions {:type (list String)}
            :id {:type Int}
            :name {:type String}
            :appears_in {:type (list :episode)}}}

  :human
  {:fields {:id {:type Int}
            :name {:type String}
            :home_planet {:type String}
            :appears_in {:type (list :episode)}}}}

 :queries
 {:hero {:type (non-null :human)
         :args {:episode {:type :episode}}
         :resolve :get-hero}
  :droid {:type :droid
          :args {:id {:type String :default-value "2001"}}
          :resolve :get-droid}}}
"
