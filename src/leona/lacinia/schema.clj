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

(defn- only-entry? [key a-map] (= [key] (keys a-map)))

(defn- simplify-all-of [spec]
  (let [subspecs (->> (:allOf spec) (remove empty?))]
    (cond
      (empty? subspecs) (dissoc spec :allOf)
      (and (= (count subspecs) 1) (only-entry? :allOf spec)) (first subspecs)
      :else (assoc spec :allOf subspecs))))

(defn- spec-dispatch [dispatch _ _ _] dispatch)

(defmulti accept-spec spec-dispatch :default ::default)

(defn transform
  ([spec]
   (transform spec nil))
  ([spec options]
   (visitor/visit spec accept-spec options)))

(defn non-null
  [t]
  (cons 'non-null [t]))

(defn list
  [t]
  (if (and (map? t) (contains? t :type))
    {:type (cons 'list [(:type t)])}
    {:type (cons 'list [t])}))

;; any? (one-of [(return nil) (any-printable)])
(defmethod accept-spec 'clojure.core/any? [_ _ _ _] {})

;; some? (such-that some? (any-printable))
(defmethod accept-spec 'clojure.core/some? [_ _ _ _] {})

;;; number? (one-of [(large-integer) (double)])
(defmethod accept-spec 'clojure.core/number? [_ _ _ _] {:type (non-null 'Float)})

(defmethod accept-spec 'clojure.core/pos? [_ _ _ _] {:minimum 0 :exclusiveMinimum true})

(defmethod accept-spec 'clojure.core/neg? [_ _ _ _] {:maximum 0 :exclusiveMaximum true})

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
(defmethod accept-spec 'clojure.core/uuid? [_ _ _ _] {:type 'ID})

;; uri? (fmap #(java.net.URI/create (str "http://" % ".com")) (uuid))
(defmethod accept-spec 'clojure.core/uri? [_ _ _ _] {:type (non-null 'String)})

;; bigdec? (fmap #(BigDecimal/valueOf %)
;;               (double* {:infinite? false :NaN? false}))
(defmethod accept-spec 'clojure.core/decimal? [_ _ _ _] {:type (non-null 'Float)})

;; inst? (fmap #(java.util.Date. %)
;;             (large-integer))
(defmethod accept-spec 'clojure.core/inst? [_ _ _ _] {:type (non-null 'String)})

;; seqable? (one-of [(return nil)
;;                   (list simple)
;;                   (vector simple)
;;                   (map simple simple)
;;                   (set simple)
;;                   (string-alphanumeric)])
(defmethod accept-spec 'clojure.core/seqable? [_ _ _ _] {:type "array"})

;; indexed? (vector simple)
(defmethod accept-spec 'clojure.core/map? [_ _ _ _] {:type "array"})

;; map? (map simple simple)
(defmethod accept-spec 'clojure.core/map? [_ _ _ _] {:type "object"})

;; vector? (vector simple)
(defmethod accept-spec 'clojure.core/vector? [_ _ _ _] {:type "array"})

;; list? (list simple)
(defmethod accept-spec 'clojure.core/list? [_ _ _ _] {:type "array"})

;; seq? (list simple)
(defmethod accept-spec 'clojure.core/seq? [_ _ _ _] {:type "array"})

;; char? (char)
(defmethod accept-spec 'clojure.core/char? [_ _ _ _] {:type (non-null 'String)})

;; set? (set simple)
(defmethod accept-spec 'clojure.core/set? [_ _ _ _] {:type "array" :uniqueItems true})

;; nil? (return nil)
(defmethod accept-spec 'clojure.core/nil? [_ _ _ _] {:type "null"})

;; false? (return false)
(defmethod accept-spec 'clojure.core/false? [_ _ _ _] {:type (non-null 'Boolean)})

;; true? (return true)
(defmethod accept-spec 'clojure.core/true? [_ _ _ _] {:type (non-null 'Boolean)})

;; zero? (return 0)
(defmethod accept-spec 'clojure.core/zero? [_ _ _ _] {:type (non-null 'Int)})

;; rational? (one-of [(large-integer) (ratio)])
(defmethod accept-spec 'clojure.core/rational? [_ _ _ _] {:type (non-null 'Float)})

;; coll? (one-of [(map simple simple)
;;                (list simple)
;;                (vector simple)
;;                (set simple)])
(defmethod accept-spec 'clojure.core/coll? [_ _ _ _] {:type "object"})

;; empty? (elements [nil '() [] {} #{}])
(defmethod accept-spec 'clojure.core/empty? [_ _ _ _] {:type "array" :maxItems 0 :minItems 0})

;; associative? (one-of [(map simple simple) (vector simple)])
(defmethod accept-spec 'clojure.core/associative? [_ _ _ _] {:type "object"})

;; sequential? (one-of [(list simple) (vector simple)])
(defmethod accept-spec 'clojure.core/sequential? [_ _ _ _] {:type "array"})

;; ratio? (such-that ratio? (ratio))
(defmethod accept-spec 'clojure.core/ratio? [_ _ _ _] {:type (non-null 'Int)})

;; bytes? (bytes)
(defmethod accept-spec 'clojure.core/ratio? [_ _ _ _] {:type (non-null 'String)})

(defmethod accept-spec ::visitor/set [dispatch spec children _]
  {:enum/values children
   :enum/name (st/spec-name spec)})

(defn title->object-name
  [t]
  (-> t ;; TODO GraphQL doesn't support namespaces so we don't really need to yet
      (name)
      (csk/->snake_case)
      (keyword)))

(s/def :enum/values (s/coll-of any?))
(s/def :enum/name keyword?)
(s/def ::enum
  (s/keys :req [:enum/values
                :enum/name]))

(defn extract-enums
  [fields]
  (let [enums (atom {})]
    (merge {:fields (walk/postwalk
                     (fn [x]
                       #_(println x)
                       (if (and (map? x)
                                (s/valid? ::enum x))
                         (let [n (:enum/name x)]
                           (swap! enums assoc (title->object-name n) {:values (:enum/values x)})
                           (non-null (title->object-name n)))
                         x)) fields)}
           (when (not-empty @enums)
             {:enums @enums}))))

(defmethod accept-spec 'clojure.spec.alpha/keys [_ spec children _]
  (let [{:keys [req req-un opt opt-un]} (impl/parse-keys (impl/extract-form spec))
        names-un (map title->object-name (concat req-un opt-un))
        names (map (comp title->object-name impl/qualified-name) (concat req opt))
        title (st/spec-name spec)
        {:keys [enums fields]} (extract-enums (zipmap (concat names names-un) children))]
    (merge {:objects (hash-map (title->object-name title)
                               (merge
                                 {:fields fields}))}
           (when enums
             {:enums enums}))))

(defmethod accept-spec 'clojure.spec.alpha/or [_ _ children _]
  {:anyOf children})

(defmethod accept-spec 'clojure.spec.alpha/and [_ _ children _]
  (simplify-all-of {:allOf children}))

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
  {:type "array", :items (impl/unwrap children), :uniqueItems true})

(defmethod accept-spec ::visitor/vector-of [_ _ children _]
  (list (impl/unwrap children)))

(defmethod accept-spec 'clojure.spec.alpha/* [_ _ children _]
  {:type "array" :items (impl/unwrap children)})

(defmethod accept-spec 'clojure.spec.alpha/+ [_ _ children _]
  {:type "array" :items (impl/unwrap children) :minItems 1})

(defmethod accept-spec 'clojure.spec.alpha/? [_ _ children _]
  {:type "array" :items (impl/unwrap children) :minItems 0})

(defmethod accept-spec 'clojure.spec.alpha/alt [_ _ children _]
  {:anyOf children})

(defmethod accept-spec 'clojure.spec.alpha/cat [_ _ children _]
  {:type "array"
   :items {:anyOf children}})

;; &

(defmethod accept-spec 'clojure.spec.alpha/tuple [_ _ children _]
  {:type "array"
   :items children})

;; keys*

(defmethod accept-spec 'clojure.spec.alpha/nilable [_ _ children _]
  {:oneOf [(impl/unwrap children) {:type "null"}]})

;;; this is just a function in clojure.spec?
(defmethod accept-spec 'clojure.spec.alpha/int-in-range? [_ spec _ _]
  (let [[_ minimum maximum _] (impl/strip-fn-if-needed spec)]
    {:minimum minimum :maximum maximum}))

(defmethod accept-spec ::visitor/spec [_ spec children _]
  (let [[_ data] (impl/extract-form spec)
        name (st/spec-name spec)
        synthetic? (-> spec st/get-spec ::st/synthetic?)
        json-schema-meta (impl/unlift-keys data "json-schema")
        extra-info (-> (select-keys data [:description])
                       (cond-> (and name (not synthetic?))
                         (assoc :title (impl/qualified-name name))))]
    (merge (impl/unwrap children) extra-info json-schema-meta)))

(defmethod accept-spec ::default [_ _ _ _]
  {})
