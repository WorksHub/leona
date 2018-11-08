(ns leona.lacinia.schema
  (:refer-clojure :exclude [list])
  (:require [camel-snake-kebab.core :as csk]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [spec-tools.core :as st]
            [spec-tools.impl :as impl]
            [spec-tools.parse :as parse]
            [spec-tools.visitor :as visitor]))

(defn qualified-gql-name->clj-name
  [t]
  (-> t
      (name)
      (str/replace #"___" "/")
      (str/replace #"__" ".")
      (csk/->kebab-case)
      (keyword)))

(defn clj-name->gql-name
  [t]
  (-> t
      (name)
      (csk/->snake_case)
      (keyword)))

(defn clj-name->qualified-gql-name
  [t]
  (let [t (if (str/starts-with? (str t) ":")
            (-> t str (subs 1))
            (str t))]
    (-> t
        (csk/->snake_case)
        (str/replace #"\." "__")
        (str/replace #"\/" "___")
        (keyword))))

(defn- only-entry? [key a-map] (= [key] (keys a-map)))

(defn- spec-dispatch [dispatch _ _ _] dispatch)

(defmulti accept-spec spec-dispatch :default ::default)

(def ^:dynamic *context* nil)

(defn non-null
  [t]
  (cons 'non-null [t]))

(defn list
  ([t]
   (list t true))
  ([t nn?]
   (if (and (map? t) (contains? t :type))
     {:type (non-null (cons 'list [(:type t)]))}
     {:type (non-null (cons 'list [t]))})))


(defn- extract-objects
  [a schema]
  (walk/postwalk
   (fn [d]
     (cond
       (and (seq? d) (= (first d) 'non-null) (map? (second d)) (contains? (second d) :type))
       (update (second d) :type non-null)
       ;;
       (and (map? d) (contains? d :objects))
       (let [k (-> d :objects keys first)]
         (swap! a assoc k (get-in d [:objects k]))
         {:type (-> d :objects keys first)})
       ;;
       :else
       d))
   schema))

(defn fix-references
  [schema]
  (let [new-objects (atom {})]
    (-> schema
        (update :objects (partial extract-objects new-objects))
        (update :objects merge @new-objects))))

(defn transform
  ([spec]
   (transform spec nil))
  ([spec options]
   (binding [*context* (atom {})]
     (let [result (visitor/visit spec accept-spec options)]
       (if (contains? @*context* :fails)
         (throw (Exception. (str"The following specs could not be transformed: " (clojure.string/join ", " (:fails @*context*)))))
         (-> result
             (second) ;; remove outer `non-null`
             (fix-references)))))))

;; any? (one-of [(return nil) (any-printable)])
(defmethod accept-spec 'clojure.core/any? [_ _ _ _] {})

;; some? (such-that some? (any-printable))
(defmethod accept-spec 'clojure.core/some? [_ _ _ _] {})

;;; number? (one-of [(large-integer) (double)])
(defmethod accept-spec 'clojure.core/number? [_ _ _ _] {:type (non-null 'Float)})

(defmethod accept-spec 'clojure.core/pos? [_ _ _ _] {:type (non-null 'Int)})

(defmethod accept-spec 'clojure.core/neg? [_ _ _ _] {:type (non-null 'Int)})

;; integer? (large-integer)
(defmethod accept-spec 'clojure.core/integer? [_ _ _ _] {:type (non-null 'Int)})

;; int? (large-integer)
(defmethod accept-spec 'clojure.core/int? [_ _ _ _] {:type (non-null 'Int)})

;; pos-int? (large-integer* {:min 1})
(defmethod accept-spec 'clojure.core/pos-int? [_ _ _ _] {:type (non-null 'Int)})

;; neg-int? (large-integer* {:max -1})
(defmethod accept-spec 'clojure.core/neg-int? [_ _ _ _] {:type (non-null 'Int)})

;; nat-int? (large-integer* {:min 0})
(defmethod accept-spec 'clojure.core/nat-int? [_ _ _ _] {:type (non-null 'Int)})

;; float? (double)
(defmethod accept-spec 'clojure.core/float? [_ _ _ _] {:type (non-null 'Float)})

;; double? (double)
(defmethod accept-spec 'clojure.core/double? [_ _ _ _] {:type (non-null 'Float)})

  ;; boolean? (boolean)
(defmethod accept-spec 'clojure.core/boolean? [_ _ _ _] {:type (non-null 'Boolean)})

  ;; string? (string-alphanumeric)
(defmethod accept-spec 'clojure.core/string? [_ _ _ _] {:type (non-null 'String)})

;; ident? (one-of [(keyword-ns) (symbol-ns)])
(defmethod accept-spec 'clojure.core/ident? [_ _ _ _] {:type (non-null 'String)})

;; simple-ident? (one-of [(keyword) (symbol)])
(defmethod accept-spec 'clojure.core/simple-ident? [_ _ _ _] {:type (non-null 'String)})

;; qualified-ident? (such-that qualified? (one-of [(keyword-ns) (symbol-ns)]))
(defmethod accept-spec 'clojure.core/qualified-ident? [_ _ _ _] {:type (non-null 'String)})

;; keyword? (keyword-ns)
(defmethod accept-spec 'clojure.core/keyword? [_ _ _ _] {:type (non-null 'String)})

;; simple-keyword? (keyword)
(defmethod accept-spec 'clojure.core/simple-keyword? [_ _ _ _] {:type (non-null 'String)})

;; qualified-keyword? (such-that qualified? (keyword-ns))
(defmethod accept-spec 'clojure.core/qualified-keyword? [_ _ _ _] {:type (non-null 'String)})

;; symbol? (symbol-ns)
(defmethod accept-spec 'clojure.core/symbol? [_ _ _ _] {:type (non-null 'String)})

;; simple-symbol? (symbol)
(defmethod accept-spec 'clojure.core/simple-symbol? [_ _ _ _] {:type (non-null 'String)})

;; qualified-symbol? (such-that qualified? (symbol-ns))
(defmethod accept-spec 'clojure.core/qualified-symbol? [_ _ _ _] {:type (non-null 'String)})

;; uuid? (uuid)
(defmethod accept-spec 'clojure.core/uuid? [_ _ _ _] {:type (non-null 'ID)})

;; uri? (fmap #(java.net.URI/create (str "http://" % ".com")) (uuid))
(defmethod accept-spec 'clojure.core/uri? [_ _ _ _] {:type (non-null 'String)})

;; bigdec? (fmap #(BigDecimal/valueOf %)
;;               (double* {:infinite? false :NaN? false}))
(defmethod accept-spec 'clojure.core/decimal? [_ _ _ _] {:type (non-null 'Float)})

;; inst? (fmap #(java.util.Date. %)
;;             (large-integer))
(defmethod accept-spec 'clojure.core/inst? [_ _ _ _] {:type (non-null 'String)})


;; char? (char)
(defmethod accept-spec 'clojure.core/char? [_ _ _ _] {:type (non-null 'String)})

;; false? (return false)
(defmethod accept-spec 'clojure.core/false? [_ _ _ _] {:type (non-null 'Boolean)})

;; true? (return true)
(defmethod accept-spec 'clojure.core/true? [_ _ _ _] {:type (non-null 'Boolean)})

;; zero? (return 0)
(defmethod accept-spec 'clojure.core/zero? [_ _ _ _] {:type (non-null 'Int)})

;; rational? (one-of [(large-integer) (ratio)])
(defmethod accept-spec 'clojure.core/rational? [_ _ _ _] {:type (non-null 'Float)})

;; ratio? (such-that ratio? (ratio))
(defmethod accept-spec 'clojure.core/ratio? [_ _ _ _] {:type (non-null 'Int)})

;; bytes? (bytes)
(defmethod accept-spec 'clojure.core/ratio? [_ _ _ _] {:type (non-null 'String)})

(defmethod accept-spec ::visitor/set [dispatch spec children _]
  (if-let [n (st/spec-name spec)]
    (do
      (swap! *context* assoc-in [:enums (clj-name->gql-name n)] {:values children})
      {:type (non-null (clj-name->gql-name n))})
    (throw (Exception. (str "Encountered a set with no name: " children "\nEnsure sets are not wrapped (with `nilable` etc)")))))

(defn remove-non-null
  [f]
  (walk/postwalk
   (fn [x]
     (if (and (seq? x) (= (first x) 'non-null))
       (second x)
       x)) f))

(defn make-optional-fields
  [fields opt opt-un]
  (let [opts (set (map clj-name->qualified-gql-name opt))
        opt-uns (set (map clj-name->gql-name opt-un))]
    (reduce-kv (fn [a k v]
                 (assoc a k
                        (if (or (contains? opts k)
                                (contains? opt-uns k))
                          (remove-non-null v)
                          v))) {} fields)))

(defmethod accept-spec 'clojure.spec.alpha/keys [_ spec children _]
  (let [{:keys [req req-un opt opt-un]} (impl/parse-keys (impl/extract-form spec))
        names-un (map clj-name->gql-name (concat req-un opt-un))
        names (map (comp clj-name->qualified-gql-name impl/qualified-name) (concat req opt))
        title (st/spec-name spec)
        fields (zipmap (concat names names-un) children)
        enums (get @*context* :enums)]
    (non-null (merge {:objects (hash-map (clj-name->gql-name title)
                                         (merge
                                           {:fields (make-optional-fields fields opt opt-un)}))}
                     (when enums
                       {:enums enums})))))

(defmethod accept-spec 'clojure.spec.alpha/or [_ _ children _]
  (throw (Exception. "GraphQL cannot represent OR logic")))

(defmethod accept-spec 'clojure.spec.alpha/and [_ spec children _]
  (if-let [t (some #(when (not= ::invalid %) %) children)]
    t
    (throw (Exception. (str "Error: 'and' must include a recognised predicate (" spec ") - " (impl/extract-form spec))))))

(defmethod accept-spec 'clojure.spec.alpha/merge [_ _ children _]
  {:type "object"
   :properties (->> (concat children
                            (mapcat :anyOf children)
                            (mapcat :allOf children))
                    (map :properties)
                    (reduce merge {}))
   :required (->> (concat children
                          (mapcat :allOf children))
                  (map :required)
                  (reduce into (sorted-set))
                  (into []))})

(defmethod accept-spec 'clojure.spec.alpha/every [_ spec children _]
  (let [form (impl/extract-form spec)
        {:keys [type]} (parse/parse-spec form)]
    (case type
      :map (hash-map (st/spec-name spec) {:additionalProperties (impl/unwrap children)})
      :set {:type "array", :uniqueItems true, :items (impl/unwrap children)}
      :vector {:type "array", :items (impl/unwrap children)})))

(defmethod accept-spec 'clojure.spec.alpha/every-kv [_ spec children _]
  (hash-map (st/spec-name spec) {:additionalProperties (second children)}))

(defmethod accept-spec ::visitor/map-of [_ spec children _]
  (hash-map (st/spec-name spec) {:additionalProperties (second children)}))

(defmethod accept-spec ::visitor/set-of [_ _ children _]
  (list (impl/unwrap children) true))

(defmethod accept-spec ::visitor/vector-of [_ _ children _]
  (list (impl/unwrap children) true))

(defmethod accept-spec 'clojure.spec.alpha/* [_ _ children _]
  (list (impl/unwrap children) true))

(defmethod accept-spec 'clojure.spec.alpha/+ [_ _ children _]
  (list (impl/unwrap children) true))

(defmethod accept-spec 'clojure.spec.alpha/? [_ _ children _]
  (list (impl/unwrap children) true))

(defmethod accept-spec 'clojure.spec.alpha/alt [_ _ children _]
  (throw (Exception. "GraphQL cannot represent OR/ALT logic")))

(defmethod accept-spec 'clojure.spec.alpha/cat [_ _ children _]
  (list (impl/unwrap children) true))

;; &

(defmethod accept-spec 'clojure.spec.alpha/tuple [_ _ children _]
  (list (impl/unwrap children) true))

;; keys*

(defmethod accept-spec 'clojure.spec.alpha/nilable [_ _ children _]
  ;; no nothing; `non-null` is controlled by req/req-un/opt/opt-un
  (impl/unwrap children))

;; ???
(defmethod accept-spec ::visitor/spec [_ spec children _]
  (let [[_ data] (impl/extract-form spec)
        name (st/spec-name spec)
        synthetic? (-> spec st/get-spec ::st/synthetic?)
        json-schema-meta (impl/unlift-keys data "json-schema")
        extra-info (-> (select-keys data [:description])
                       (cond-> (and name (not synthetic?))
                         (assoc :title (impl/qualified-name name))))]
    (merge (impl/unwrap children) extra-info json-schema-meta)))

(defmethod accept-spec ::default [_ spec _ _]
  (when spec
    (swap! *context* update :fails conj spec)
    ::invalid))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn combine
  [& specs]
  (apply merge-with merge (map transform specs)))
