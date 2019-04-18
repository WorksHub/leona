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

(defn- split-keep-delim 
  [s re-delim]
  (let [m (.matcher re-delim s)]
    ((fn step [last-end]
       (if (.find m)
         (let [start (.start m)
               end (.end m)
               delim (.group m)
               new-head (if (not= last-end start)
                          [(.substring s last-end start) delim]
                          [delim])]
           (concat new-head (lazy-seq (step end))))
         (if (not= last-end (.length s))
           [(.substring s last-end)]
           []))) 0)))

(defn- PascalCase
  [s]
  (str/join ""
    (map (fn [t] (if (str/starts-with? (str t) "_")
                    (str t)
                    (csk/->PascalCase t)))
      (split-keep-delim s #"[__|___]"))))

(defn- kebab-case
  [s]
  (str/join ""
    (map (fn [t] (if (contains? (set "./") (str t))
                    (str t)
                    (csk/->kebab-case t)))
      (split-keep-delim s #"[.|/]"))))

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
        (str/replace #"\." "__")
        (str/replace #"\/" "___")
        (PascalCase)
        (replace-punctuation)
        (keyword))))

(defn gql-name->clj-name
  [t]
  (-> t
      (name)
      (str/replace #"___" "/")
      (str/replace #"__" ".")
      (replace-placeholders)
      (kebab-case)
      (keyword)))
