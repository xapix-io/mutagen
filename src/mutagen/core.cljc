(ns mutagen.core
  #?(:cljs (:require [clojure.string :as string]
                     [goog.string :as gstring])))

;; === Common combinators ===

(defn- -eps
  "Always succeed without consuming any input"
  []
  (fn []
    (fn [in out ok _fail]
      #(ok in out))))

(defn- -cat
  ([P]
   (fn []
     (fn [in out ok fail]
       ((P) in out ok fail))))
  ([P & PS]
   (reduce
    (fn [P Q]
      (fn []
        (fn [in out ok fail]
          ((P) in out
           (fn [in out]
             #((Q) in out ok fail))
           fail))))
    (list* P PS))))

(defn- -alt
  ([P]
   (fn []
     (fn [in out ok fail]
       ((P) in out ok fail))))
  ([P & PS]
   (reduce
    (fn [P Q]
      (fn []
        (fn [in out ok fail]
          ((P) in out ok
           (fn [_ _]
             #((Q) in out ok fail))))))
    (list* P PS))))

(defn- -star [P]
  (fn -star* []
    (fn [in out ok fail]
      (if (empty? in)
        (ok in out)
        ((P) in out
         (fn [in out]
           #((-star*) in out ok fail))
         (fn [_ _]
           #(ok in out)))))))

(defn- -plus [P]
  (let [P* (-star P)]
    (fn []
      (fn [in out ok fail]
        ((P) in out
         (fn [in out]
           #((P*) in out ok fail))
         fail)))))

(defn- -opt [P]
  (fn []
    (fn [in out ok _fail]
      ((P) in out ok ok))))

(defn- -rep [times P]
  (let [PS (map (fn [_i]
                  (fn []
                    (fn [in out ok fail]
                      ((P) in out
                       (fn [in out]
                         #(ok in out))
                       (fn [in out]
                         ;; TODO add exception
                         #(fail in out))))))
                (range times))]
    (apply -cat PS)))

(defn- -la [P]
  (fn []
    (fn [in out ok fail]
      ((P) in out
       (fn [_ _]
         #(ok in out))
       (fn [_ _]
         #(fail in out))))))

(defn- -neg-la [P]
  (fn []
    (fn [in out ok fail]
      ((P) in out
       (fn [_ _]
         #(fail in out))
       (fn [_ _]
         #(ok in out))))))

(defn- -skip [P]
  (fn []
    (fn [in out ok fail]
      ((P) in out
       (fn [in _]
         #(ok in out))
       fail))))

(defn- -keep [P]
  (fn []
    (fn [in out ok fail]
      ((P) in out
       (fn [_ out]
         #(ok in out))
       fail))))

;; === String terminals ===

(defn- -char [& chs]
  (let [chs (set chs)]
    (fn []
      (fn [in out ok fail]
        (let [ch (first in)]
          (if (contains? chs ch)
            (ok (rest in) (conj out ch))
            (fail in out)))))))

(defn- -alpha-char [except]
  (let [except (set except)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if (and #?(:clj (Character/isLetter ch)
                      :cljs (not= (string/lower-case ch)
                                  (string/upper-case ch)))
                   (not (contains? except ch)))
            (ok (rest in) (conj out ch))
            (fail in out))
          (fail in out))))))

(defn- -alpha-num-char [except]
  (let [except (set except)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if (and #?(:clj (Character/isLetterOrDigit ch)
                      :cljs (or (not= (string/lower-case ch)
                                      (string/upper-case ch))
                                (gstring/isNumeric ch)))
                   (not (contains? except ch)))
            (ok (rest in) (conj out ch))
            (fail in out))
          (fail in out))))))

(defn- -word [w]
  (let [w (seq w)
        size (count w)]
    (fn []
      (fn [in out ok fail]
        (let [chs (take size in)]
          (if (= w chs)
            (ok (drop size in) (into out chs))
            (fail in out)))))))

(defn- -any-char [except]
  (let [except (set except)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if-not (contains? except ch)
            (ok (rest in) (conj out ch))
            (fail in out))
          (fail in out))))))

(defn- -ws-char [except]
  (let [except (set except)
        chs (into #{\space \backspace \formfeed \newline \return \tab} except)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if (contains? chs ch)
            (ok (rest in) (conj out ch))
            (fail in out))
          (fail in out))))))

(defn- -end-of-string []
  (fn []
    (fn [in out ok fail]
      (if (empty? in)
        (ok in out)
        (fail in out)))))

(declare parser)

(defmulti vec->parser* (fn [_opts v] (first v)))

(defmethod vec->parser* :default [{:keys [registry] :as _opts} [ref _props _children]]
  (fn []
    ((get @registry ref))))

(defmethod vec->parser* ::eps [_opts [_ _props]]
  (-eps))

(defmethod vec->parser* ::cat [opts [_ _props children]]
  (let [children (mapv (partial parser opts) children)]
    (apply -cat children)))

(defmethod vec->parser* ::alt [opts [_ _props children]]
  (let [children (mapv (partial parser opts) children)]
    (apply -alt children)))

(defmethod vec->parser* ::plus [opts [_ _props [child]]]
  (let [child (parser opts child)]
    (-plus child)))

(defmethod vec->parser* ::star [opts [_ _props [child]]]
  (let [child (parser opts child)]
    (-star child)))

(defmethod vec->parser* ::opt [opts [_ _props [child]]]
  (let [child (parser opts child)]
    (-opt child)))

(defmethod vec->parser* ::rep [opts [_ {:keys [times] :as _props} [child]]]
  (let [child (parser opts child)]
    (-rep times child)))

(defmethod vec->parser* ::la [opts [_ _props [child]]]
  (let [child (parser opts child)]
    (-la child)))

(defmethod vec->parser* ::nla [opts [_ _props [child]]]
  (let [child (parser opts child)]
    (-neg-la child)))

(defmethod vec->parser* ::skip [opts [_ _props [child]]]
  (let [child (parser opts child)]
    (-skip child)))

(defmethod vec->parser* ::keep [opts [_ _props [child]]]
  (let [child (parser opts child)]
    (-keep child)))

(defmethod vec->parser* ::char [_opts [_ _props chs]]
  (apply -char chs))

(defmethod vec->parser* ::any-char [_opts [_ {:keys [except]} _]]
  (-any-char except))

(defmethod vec->parser* ::alpha [_opts [_ {:keys [except]} _]]
  (-alpha-char except))

(defmethod vec->parser* ::alpha-num [_opts [_ {:keys [except]} _]]
  (-alpha-num-char except))

(defmethod vec->parser* ::ws [_opts [_ {:keys [except]} _]]
  (-ws-char except))

(defmethod vec->parser* ::word [_opts [_ _props [w]]]
  (-word w))

(defmethod vec->parser* ::eof [_opts [_ _props _]]
  (-end-of-string))

(defn- -wrap-result [P wrap-fn]
  (fn []
    (fn [in out ok fail]
      ((P) in (empty out)
       (fn [in out']
         #(ok in (conj out (wrap-fn out'))))
       fail))))

(defn vec->parser [opts vector-form]
  (let [[t & children] vector-form
        {:keys [wrap hide] :as props}
        (if (map? (first children))
          (first children)
          {})
        children
        (if (map? (first children))
          (rest children)
          children)]
    (cond-> (vec->parser* opts [t props children])
      wrap (-wrap-result wrap)
      hide (-skip))))

(defn keyword->parser [opts k]
  (vec->parser opts [k {}]))

(defn parser
  ([form] (parser {:registry (volatile! {})} form))
  ([opts form]
   (cond
     (vector? form)
     (vec->parser opts form)

     (keyword? form)
     (keyword->parser opts form))))

(defprotocol Grammar
  (-parser [this start-production-rule]))

(defn- default-ok [_in out]
  out)

(defn- default-fail [in out]
  (throw (ex-info "Can not parse input" {:in in :out out})))

(defn grammar
  ([grammar-map] (grammar {:registry (atom {})} grammar-map))
  ([opts grammar-map]
   (let [{:keys [registry]}
         (reduce-kv
          (fn [{:keys [registry] :as opts} k form]
            (let [p (parser opts form)]
              (if (contains? @registry k)
                (throw (ex-info "Parser already defined" {:ref k}))
                (swap! registry assoc k p))
              opts))
          opts
          grammar-map)]
     (reify Grammar
       (-parser [_ start-production-rule]
         (if-let [P (get @registry start-production-rule)]
           (fn f
             ([in] (f in [] default-ok default-fail))
             ([in out] (f in out default-ok default-fail))
             ([in out ok] (f in out ok default-fail))
             ([in out ok fail]
              (trampoline (P) in out ok fail)))
           (throw (ex-info "Unknown production rule" {:rule start-production-rule}))))))))
