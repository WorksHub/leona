(ns leona.schema
  (:refer-clojure :exclude [list])
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [leona.util :as util]
            [spec-tools.core :as st]
            [spec-tools.impl :as impl]
            [spec-tools.visitor :as visitor]))


(def valid-type-override-syms
  "These symbols can be used to wrap types when using `st/spec`, e.g. `(st/spec my-date-pred? {:type '(custom :date)})`. They only exist separately to provide semantic information - it's not asserted anywhere that the types they reference ever exist anywhere so USE WITH CAUTION."
  #{'enum 'custom})

(defn- only-entry? [key a-map] (= [key] (keys a-map)))

(defn- spec-dispatch [dispatch _ _ _] dispatch)

(defmulti accept-spec spec-dispatch :default ::default)

(def ^:dynamic *context* nil)

(defn spec-resolves-to-set?
  [spec]
  (some-> spec s/get-spec s/form resolve deref set?))

(defn non-null
  [t]
  (cons 'non-null [t]))

(defn list
  ([t s]
   (list t s true))
  ([t s nn?]
   (if (and (map? t) (contains? t :type))
     {:type (non-null (cons 'list [(:type t)]))
      :spec (or s (:spec t))}
     {:type (non-null (cons 'list [t]))
      :spec s})))

(defn spec-name-or-alias
  [spec {:keys [type-aliases]}]
  (get type-aliases spec (some-> spec (st/spec-name) (util/clj-name->gql-object-name))))

(defn find-invalid-key
  ([m path]
   (when (map? m) ;; TODO what if it's not a map?
     (some (fn [[k v]]
             (cond
               (= ::invalid v) {:field k :path path}
               (map? v)        (find-invalid-key v (conj path k))
               :else           nil))
           m)))
  ([m]
   (find-invalid-key m [])))

(defn clean-extracted-object
  [d k]
  (let [m (get-in d [:objects k])]
    (if (and (contains? m :spec)
             (contains? m :ref))
      (-> m
          (dissoc :spec)
          (set/rename-keys {:ref :spec}))
      m)))

(defn- extract-objects
  [options extracted-objects schema]
  (walk/postwalk
   (fn [d]
     (cond
       (and (seq? d) (= (first d) 'non-null) (map? (second d)) (contains? (second d) :type))
       (update (second d) :type non-null)
       ;;
       (and (map? d) (contains? d :objects))
       (let [k    (-> d :objects keys first)
             spec (get-in d [:objects k :ref])
             ref  (some-> spec (spec-name-or-alias options))
             k'   (or ref k)]
         (swap! extracted-objects assoc k' (clean-extracted-object d k))
         {:type k'
          :spec (get-in d [:objects k :spec])})
       ;;
       :else
       d))
   schema))

(defn- fix-lists
  "Attempts to find types inside lists and removes the inner type map"
  [schema]
  (walk/postwalk
   (fn [d]
     (cond
       (and (seq? d) (= (first d) 'list) (map? (second d)) (contains? (second d) :type))
       (clojure.core/list 'list (:type (second d)))
       ;;
       :else d))
   schema))

(defn fix-references
  [schema options]
  (let [new-objects (atom {})]
    (-> schema
        (update :objects (partial extract-objects options new-objects))
        (update :objects #(merge @new-objects %))
        (update :objects fix-lists))))

(defn accept-spec-wrapper
  "We use this function to intercept calls to 'accept-spec' and skip certain specs (e.g. where they are custom scalars)"
  [dispatch spec children {:keys [custom-scalars] :as opts}]
  (if (contains? custom-scalars spec)
    {:type (non-null (util/clj-name->gql-object-name spec))
     :spec spec}
    (accept-spec dispatch spec children opts)))

(defn transform
  ([spec]
   (transform spec nil))
  ([spec options]
   (binding [*context* (atom {})]
     (let [result (-> spec
                      (visitor/visit accept-spec-wrapper options)
                      (second)
                      (fix-references options))]
       (if-let [field (find-invalid-key result)]
         (throw (Exception. (str "Spec could not be transformed: " field))) ;; TODO improve this error
         result)))))

;; nil? special case for no-args queries
(defmethod accept-spec 'clojure.core/nil? [_ spec _ opts]
  (let [title (spec-name-or-alias spec opts)]
    (non-null {:objects {title {:fields {}}}})))

;; any? (one-of [(return nil) (any-printable)])
(defmethod accept-spec 'clojure.core/any? [_ _ _ _] {})

;; some? (such-that some? (any-printable))
(defmethod accept-spec 'clojure.core/some? [_ _ _ _] {})

;;; number? (one-of [(large-integer) (double)])
(defmethod accept-spec 'clojure.core/number? [_ spec _ _] {:type (non-null 'Float)
                                                           :spec spec})

(defmethod accept-spec 'clojure.core/pos? [_ spec _ _] {:type (non-null 'Int)
                                                        :spec spec})

(defmethod accept-spec 'clojure.core/neg? [_ spec _ _] {:type (non-null 'Int)
                                                        :spec spec})

;; integer? (large-integer)
(defmethod accept-spec 'clojure.core/integer? [_ spec _ _] {:type (non-null 'Int)
                                                            :spec spec})

;; int? (large-integer)
(defmethod accept-spec 'clojure.core/int? [_ spec _ _]
  {:type (non-null 'Int)
   :spec spec})

;; pos-int? (large-integer* {:min 1})
(defmethod accept-spec 'clojure.core/pos-int? [_ spec _ _] {:type (non-null 'Int)
                                                            :spec spec})

;; neg-int? (large-integer* {:max -1})
(defmethod accept-spec 'clojure.core/neg-int? [_ spec _ _] {:type (non-null 'Int)
                                                            :spec spec})

;; nat-int? (large-integer* {:min 0})
(defmethod accept-spec 'clojure.core/nat-int? [_ spec _ _] {:type (non-null 'Int)
                                                            :spec spec})

;; float? (double)
(defmethod accept-spec 'clojure.core/float? [_ spec _ _] {:type (non-null 'Float)
                                                          :spec spec})

;; double? (double)
(defmethod accept-spec 'clojure.core/double? [_ spec _ _] {:type (non-null 'Float)
                                                           :spec spec})

;; boolean? (boolean)
(defmethod accept-spec 'clojure.core/boolean? [_ spec _ _] {:type (non-null 'Boolean)
                                                            :spec spec})

;; string? (string-alphanumeric)
(defmethod accept-spec 'clojure.core/string? [_ spec _ _] {:type (non-null 'String)
                                                           :spec spec})

;; ident? (one-of [(keyword-ns) (symbol-ns)])
(defmethod accept-spec 'clojure.core/ident? [_ spec _ _] {:type (non-null 'String)
                                                          :spec spec})

;; simple-ident? (one-of [(keyword) (symbol)])
(defmethod accept-spec 'clojure.core/simple-ident? [_ spec _ _] {:type (non-null 'String)
                                                                 :spec spec})

;; qualified-ident? (such-that qualified? (one-of [(keyword-ns) (symbol-ns)]))
(defmethod accept-spec 'clojure.core/qualified-ident? [_ spec _ _] {:type (non-null 'String)
                                                                    :spec spec})

;; keyword? (keyword-ns)
(defmethod accept-spec 'clojure.core/keyword? [_ spec _ _] {:type (non-null 'String)
                                                            :spec spec})

;; simple-keyword? (keyword)
(defmethod accept-spec 'clojure.core/simple-keyword? [_ spec _ _] {:type (non-null 'String)
                                                                   :spec spec})

;; qualified-keyword? (such-that qualified? (keyword-ns))
(defmethod accept-spec 'clojure.core/qualified-keyword? [_ spec _ _] {:type (non-null 'String)
                                                                      :spec spec})

;; symbol? (symbol-ns)
(defmethod accept-spec 'clojure.core/symbol? [_ spec _ _] {:type (non-null 'String)
                                                           :spec spec})

;; simple-symbol? (symbol)
(defmethod accept-spec 'clojure.core/simple-symbol? [_ spec _ _] {:type (non-null 'String)
                                                                  :spec spec})

;; qualified-symbol? (such-that qualified? (symbol-ns))
(defmethod accept-spec 'clojure.core/qualified-symbol? [_ spec _ _] {:type (non-null 'String)
                                                                     :spec spec})

;; uuid? (uuid)
(defmethod accept-spec 'clojure.core/uuid? [_ spec _ _] {:type (non-null 'ID)
                                                         :spec spec})

;; uri? (fmap #(java.net.URI/create (str "http://" % ".com")) (uuid))
(defmethod accept-spec 'clojure.core/uri? [_ spec _ _] {:type (non-null 'String)
                                                        :spec spec})

;; bigdec? (fmap #(BigDecimal/valueOf %)
;;               (double* {:infinite? false :NaN? false}))
(defmethod accept-spec 'clojure.core/decimal? [_ spec _ _] {:type (non-null 'Float)
                                                            :spec spec})

;; inst? (fmap #(java.util.Date. %)
;;             (large-integer))
(defmethod accept-spec 'clojure.core/inst? [_ spec _ _] {:type (non-null 'String)
                                                         :spec spec})


;; char? (char)
(defmethod accept-spec 'clojure.core/char? [_ spec _ _] {:type (non-null 'String)
                                                         :spec spec})

;; false? (return false)
(defmethod accept-spec 'clojure.core/false? [_ spec _ _] {:type (non-null 'Boolean)
                                                          :spec spec})

;; true? (return true)
(defmethod accept-spec 'clojure.core/true? [_ spec _ _] {:type (non-null 'Boolean)
                                                         :spec spec})

;; zero? (return 0)
(defmethod accept-spec 'clojure.core/zero? [_ spec _ _] {:type (non-null 'Int)
                                                         :spec spec})

;; rational? (one-of [(large-integer) (ratio)])
(defmethod accept-spec 'clojure.core/rational? [_ spec _ _] {:type (non-null 'Float)
                                                             :spec spec})

;; ratio? (such-that ratio? (ratio))
(defmethod accept-spec 'clojure.core/ratio? [_ spec _ _] {:type (non-null 'Int)
                                                          :spec spec})

;; bytes? (bytes)
(defmethod accept-spec 'clojure.core/ratio? [_ spec _ _] {:type (non-null 'String)
                                                          :spec spec})

(defmethod accept-spec ::visitor/set [dispatch spec children opts]
  (if-let [n (spec-name-or-alias spec opts)]
    (do
      (swap! *context* assoc-in [:enums n] {:values (vec (map util/clj-name->gql-enum-name children))})
      {:type (non-null n)
       :spec spec})
    (throw (Exception. (str "Encountered a set with no name: " spec "\nEnsure sets are not wrapped (with `nilable` etc)")))))

(defn remove-non-null
  [f]
  (let [lock? (atom false)]
    (walk/prewalk
     (fn [x]
       (when (or (and (map? x) (contains? x :objects))
                 (and (vector? x) (= (first x) :objects)))
         (reset! lock? true))
       (cond
         @lock? x
         (and (seq? x) (= (first x) 'non-null))
         (second x)
         :else x)) f)))

(defn make-optional-fields
  ([fields spec]
   (let [{:keys [opt opt-un]} (impl/parse-keys (impl/extract-form spec))]
     (if (or opt opt-un)
       (make-optional-fields fields opt opt-un)
       fields)))
  ([fields opt opt-un]
   (let [opts    (set (map util/clj-name->qualified-gql-name opt))
         opt-uns (set (map util/clj-name->gql-name opt-un))]
     (reduce-kv (fn [a k v]
                  (let [contained? (or (contains? opts k)
                                       (contains? opt-uns k))
                        object?    (and (sequential? v)
                                        (= 'non-null (first v))
                                        (map? (second v))
                                        (contains? (second v) :objects))]
                    (assoc a k
                           (cond
                             (not contained?) v
                             object?          (second v)
                             :else            (remove-non-null v)))))
                {} fields))))

(defmethod accept-spec 'clojure.spec.alpha/keys [_ spec children opts]
  (let [{:keys [req req-un opt opt-un]} (impl/parse-keys (impl/extract-form spec))
        names-un (map util/clj-name->gql-name (concat req-un opt-un))
        names (map (comp util/clj-name->qualified-gql-name impl/qualified-name) (concat req opt))
        title (spec-name-or-alias spec opts)
        fields (zipmap (concat names names-un) children)
        enums (get @*context* :enums)
        spec-ref (s/get-spec spec)]
    (if title
      (non-null (merge {:objects (hash-map title
                                           (if (keyword? spec-ref)
                                             {:fields (make-optional-fields fields spec-ref)
                                              :spec spec
                                              :ref spec-ref}
                                             {:fields (make-optional-fields fields opt opt-un)
                                              :spec spec}))}
                       (when enums
                         {:enums enums})))
      (throw (Exception. (str "Cannot process anonymous `s/keys` specs. Please provide a name: " spec) )))))

(defmethod accept-spec 'clojure.spec.alpha/or [_ spec children _]
  (if-let [t (some #(when (not= ::invalid %) %) children)]
    t
    (throw (Exception. (str "Error: 's/or' must include a recognised predicate (" spec ") - " (impl/extract-form spec))))))

(defmethod accept-spec 'clojure.spec.alpha/and [_ spec children _]
  (if-let [t (some #(when (not= ::invalid %) %) children)]
    t
    (throw (Exception. (str "Error: 's/and' must include a recognised predicate (" spec ") - " (impl/extract-form spec))))))

(defmethod accept-spec 'clojure.spec.alpha/double-in [_ spec _ _] {:type (non-null 'Float)
                                                                   :spec spec})

(defmethod accept-spec 'clojure.spec.alpha/int-in [_ _ _ _] {:type (non-null 'Int)})

(defmethod accept-spec 'clojure.spec.alpha/merge [_ spec children opts]
  (let [objects (not-empty (apply merge (map (comp :fields second first :objects second) children)))
        enums   (not-empty (apply merge (map (comp :enums second) children)))
        name    (spec-name-or-alias spec opts)]
    (non-null (merge {:objects (hash-map name {:fields objects
                                               :spec spec})}
                     (when enums
                       {:enums enums})))))

(defmethod accept-spec 'clojure.spec.alpha/every [_ spec children _]
  ::invalid)

(defmethod accept-spec 'clojure.spec.alpha/every-kv [_ spec children _]
  ::invalid)

(defmethod accept-spec ::visitor/map-of [_ spec children _]
  ::invalid)

(defmethod accept-spec ::visitor/set-of [_ spec children _]
  (list (impl/unwrap children) spec true))

(defmethod accept-spec ::visitor/vector-of [_ spec children _]
  (list (impl/unwrap children) spec true))

(defmethod accept-spec 'clojure.spec.alpha/* [_ spec children _]
  (list (impl/unwrap children) spec true))

(defmethod accept-spec 'clojure.spec.alpha/+ [_ spec children _]
  (list (impl/unwrap children) spec true))

(defmethod accept-spec 'clojure.spec.alpha/? [_ spec children _]
  (list (impl/unwrap children) spec true))

(defmethod accept-spec 'clojure.spec.alpha/alt [_ _ children _]
  (throw (Exception. "GraphQL cannot represent OR/ALT logic")))

(defmethod accept-spec 'clojure.spec.alpha/cat [_ spec children _]
  (list (impl/unwrap children) spec true))

;; &

(defmethod accept-spec 'clojure.spec.alpha/tuple [_ spec children _]
  (list (impl/unwrap children) spec true))

;; keys*

(defmethod accept-spec 'clojure.spec.alpha/nilable [_ spec children _]
  ;; no nothing; `non-null` is controlled by req/req-un/opt/opt-un
  (let [r (impl/unwrap children)]
    (cond (map? r)
          (assoc r :spec spec)
          :else
          (throw (Exception. "whoa, what's this???")))))

(s/def ::replacement-types (s/+ #{'String 'Float 'Int 'Boolean 'ID
                                  'non-null 'list}))

(defn valid-replacement-type?
  [t]
  (if t
    (let [flat (flatten [t])]
      (if (valid-type-override-syms (first flat))
        (keyword? (second flat))
        (s/valid? ::replacement-types flat)))
    false))

;; ???
(defmethod accept-spec ::visitor/spec [_ spec children opts]
  (let [[_ data] (impl/extract-form spec)
        replacement-type (:type data)
        un-children (impl/unwrap children)]
    (merge
     (if (valid-replacement-type? replacement-type)
       (let [replacement-type' (if (and (seq? replacement-type) ;; extract valid type override fn if we have one
                                        (valid-type-override-syms (first replacement-type)))
                                 (second replacement-type)
                                 replacement-type)]
         (if (map? un-children)
           (assoc un-children
                  :type replacement-type'
                  :spec spec)
           {:type replacement-type'
            :spec spec}))
       un-children)
     (select-keys data [:description]))))

(defmethod accept-spec ::default [_ spec _ opts]
  (try
    (if spec-resolves-to-set?
      (accept-spec ::visitor/set spec (-> spec s/get-spec s/form resolve deref) opts)
      (when spec ::invalid))
    (catch Exception _
      (when spec ::invalid))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn combine
  [& specs]
  (apply merge-with merge (map transform specs)))

(defn combine-with-opts
  [opts & specs]
  (apply merge-with merge (map #(transform % opts) specs)))
