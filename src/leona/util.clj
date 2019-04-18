(ns leona.util
  (:require [camel-snake-kebab.core :as csk]
            [clojure.string :as str]))

(defn replace-punctuation
  [s]
  (-> s
      (str/replace #"\?" "_QMARK_")
      (str/replace #"!" "_XMARK_")))

(defn replace-placeholders
  [s]
  (-> s
      (str/replace #"_QMARK_" "?")
      (str/replace #"_XMARK_" "!")))

(defn clj-name->gql-name
  [t]
  (-> t
      (name)
      (csk/->PascalCase)
      (replace-punctuation)
      (keyword)))

(defn clj-name->qualified-gql-name
  [t]
  (let [t (if (str/starts-with? (str t) ":")
            (-> t str (subs 1))
            (str t))]
    (-> t
        (csk/->PascalCase)
        (str/replace #"\." "__")
        (str/replace #"\/" "___")
        (replace-punctuation)
        (keyword))))

(defn gql-name->clj-name
  [t]
  (-> t
      (name)
      (str/replace #"___" "/")
      (str/replace #"__" ".")
      (replace-placeholders)
      (csk/->kebab-case)
      (keyword)))
