(ns leona.util
  (:require [camel-snake-kebab.core :as csk]
            [clojure.spec.alpha :as s]
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

(defn- case-change-impl
  [f s]
  (str/join ""
            (map (fn [t] (if (str/starts-with? (str t) "_")
                           (str t)
                           (f t)))
                 (split-keep-delim s #"[__|___]"))))

(def PascalCase
  (partial case-change-impl csk/->PascalCase))

(def camelCase
  (partial case-change-impl csk/->camelCase))

(def snake_case
  (partial case-change-impl csk/->snake_case))

(def SCREAMING_SNAKE_CASE
  (partial case-change-impl csk/->SCREAMING_SNAKE_CASE))

(defn- kebab-case
  [s]
  (str/join ""
            (map (fn [t] (if (contains? (set "./") (str t))
                           (str t)
                           (csk/->kebab-case t)))
                 (split-keep-delim s #"[.|/]"))))

(defn clj-name->gql-name-impl
  [f t]
  (-> t
      (name)
      (f)
      (replace-punctuation)
      (keyword)))

(defn clj-name->qualified-gql-name-impl
  [f t]
  (let [t (if (str/starts-with? (str t) ":")
            (-> t str (subs 1))
            (str t))
        t' (-> t
               (str/replace #"\." "__")
               (str/replace #"\/" "___"))
        [start end] (str/split t' #"___")]
    (if end
      (-> (str (snake_case start)
               "___"
               (-> end
                   (f)
                   (replace-punctuation)))
          (keyword))
      (-> start
          (f)
          (replace-punctuation)
          (keyword)))))

(def clj-name->gql-name
  (partial clj-name->gql-name-impl camelCase))

(def clj-name->gql-object-name
  (partial clj-name->gql-name-impl PascalCase))

(def clj-name->gql-enum-name
  (partial clj-name->gql-name-impl SCREAMING_SNAKE_CASE))

(def clj-name->qualified-gql-name
  (partial clj-name->qualified-gql-name-impl camelCase))

(def clj-name->qualified-gql-object-name
  (partial clj-name->qualified-gql-name-impl PascalCase))

(def clj-name->qualified-gql-enum-name
  (partial clj-name->qualified-gql-name-impl SCREAMING_SNAKE_CASE))


(defn gql-name->clj-name
  [t]
  (-> t
      (name)
      (str/replace #"___" "/")
      (str/replace #"__" ".")
      (replace-placeholders)
      (kebab-case)
      (keyword)))

(defn find-case-match
  "In this fn we attempt to contains? k on m (map or set), but we spin through some cases and return the one that fits"
  ([m t] ;; this is simply necessary to keep csk loaded
   (find-case-match
     m t [csk/->camelCase
          csk/->PascalCase
          csk/->SCREAMING_SNAKE_CASE
          csk/->snake_case
          csk/->kebab-case]))
  ([m t fns]
   (when-let [case-fn (first fns)]
     (let [s (if (str/starts-with? (str t) ":")
               (-> t str (subs 1))
               (str t))
           new-kw (keyword (case-fn s))]
       (if (contains? m new-kw)
         new-kw
         (find-case-match m t (rest fns)))))))

(defn spec-keys
  [s]
  (let [form (s/form s)]
    (when (and (sequential? form)
               (= (first form) 'clojure.spec.alpha/keys))
      (->> form
           (rest)
           (rest)
           (take-nth 2)
           (reduce concat)))))

(defn remove-ns*
  [kw]
  (keyword (name kw)))

(defn get*
  [m kw]
  (or (get m kw)
      (get m (remove-ns* kw))))
