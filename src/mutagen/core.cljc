(ns mutagen.core
  #?(:cljs (:require [clojure.string :as string]
                     [goog.string :as gstring])))

;; === Common combinators ===

(defn- -eps
  "Always succeed without consuming any input"
  []
  (fn []
    (fn -success [in out ok _fail]
      #(ok in out))))

(defn- -fail
  [failure]
  (fn []
    (fn -failure [in out _ok fail]
      #(fail in out failure))))

(defn- -cat [form PS]
  (reduce
   (fn [P Q]
     (fn []
       (fn -partial-cat [in out ok fail]
         ((P) in out
          (fn [in out]
            #((Q) in out ok
              (fn [in out failure]
                (fn []
                  (fail in out {:cause failure
                                :form form
                                :type ::cat-failure})))))
          fail))))
   (list* (-eps) PS)))

(defn- -alt [form PS]
  (reduce
   (fn [P Q]
     (fn []
       (fn [in out ok fail]
         ((P) in out ok
          (fn [_ _ P-failure]
            #((Q) in out ok
              (fn [in out Q-failure]
                (fn []
                  (fail in out (update P-failure :cause conj Q-failure))))))))))
   (list* (-fail {:type ::alt-failure
                  :cause []
                  :form form}) PS)))

(defn- -star [P]
  (fn -star* []
    (fn [in out ok fail]
      (if (empty? in)
        (ok in out)
        ((P) in out
         (fn [in out]
           #((-star*) in out ok fail))
         (fn [_ _ _]
           #(ok in out)))))))

(defn- -plus [form P]
  (let [P* (-star P)]
    (fn []
      (fn [in out ok fail]
        ((P) in out
         (fn [in out]
           #((P*) in out ok fail))
         (fn [in out failure]
           #(fail in out {:type ::plus-error
                          :cause failure
                          :form form})))))))

(defn- -opt [_form P]
  (fn []
    (fn [in out ok _fail]
      ((P) in out ok
       (fn [_in _out _failure]
         #(ok in out))))))

(defn- -rep [form times P]
  (reduce
   (fn [Q i]
     (fn []
       (fn [in out ok fail]
         ((Q) in out
          (fn [in out]
            #((P) in out ok
              (fn [in out failure]
                (fn []
                  (fail in out {:type ::rep-error
                                :actual i
                                :form form
                                :cause failure})))))
          fail))))
   (-eps)
   (range times)))

(defn- -la [form P]
  (fn []
    (fn [in out ok fail]
      ((P) in out
       (fn [_ _]
         #(ok in out))
       (fn [_ _ failure]
         #(fail in out {:type ::la-error
                        :form form
                        :cause failure}))))))

(defn- -neg-la [form P]
  (fn []
    (fn [in out ok fail]
      ((P) in out
       (fn [_ _]
         #(fail in out {:type ::nla-error
                        :form form}))
       (fn [_ _ _]
         #(ok in out))))))

(defn- -skip [form P]
  (fn []
    (fn [in out ok fail]
      ((P) in out
       (fn [in _]
         #(ok in out))
       (fn [in out failure]
         #(fail in out {:type ::skip-error
                        :cause failure
                        :form form}))))))

(defn- -keep [form P]
  (fn []
    (fn [in out ok fail]
      ((P) in out
       (fn [_ out]
         #(ok in out))
       (fn [in out failure]
         #(fail in out {:type ::keep-error
                        :cause failure
                        :form form}))))))

;; === String terminals ===

(defn- -char [form chs]
  (let [chs (set chs)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if (contains? chs ch)
            (ok (rest in) (conj out ch))
            (fail in out {:form form
                          :actual ch
                          :type ::unexpected-char}))
          (fail in out {:form form
                        :type ::unexpected-eof}))))))

(defn alpha-char? [ch]
  #?(:clj (Character/isLetter ch)
     :cljs (not= (string/lower-case ch)
                 (string/upper-case ch))))

(defn alpha-num-char? [ch]
  #?(:clj (Character/isLetterOrDigit ch)
     :cljs (or (not= (string/lower-case ch)
                     (string/upper-case ch))
               (gstring/isNumeric ch))))

(defn- -alpha-char [form except]
  (let [except (set except)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if (and (alpha-char? ch)
                   (not (contains? except ch)))
            (ok (rest in) (conj out ch))
            (fail in out {:form form
                          :actual ch
                          :type ::unexpected-char}))
          (fail in out {:form form
                        :type ::unexpected-eof}))))))

(defn- -alpha-num-char [form except]
  (let [except (set except)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if (and (alpha-num-char? ch)
                   (not (contains? except ch)))
            (ok (rest in) (conj out ch))
            (fail in out {:form form
                          :actual ch
                          :type ::unexpected-char}))
          (fail in out {:form form
                        :type ::unexpected-eof}))))))

(defn- -word [form w]
  (let [w (seq w)
        size (count w)]
    (fn []
      (fn [in out ok fail]
        (let [chs (take size in)]
          (cond
            (not= size (count chs))
            (fail in out {:form form
                          :type ::unexpected-eof})

            (= w chs)
            (ok (drop size in) (into out chs))

            :else
            (fail in out {:form form
                          :type ::unexpected-char
                          :actual (apply str chs)})))))))

(defn- -any-char [form except]
  (let [except (set except)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if-not (contains? except ch)
            (ok (rest in) (conj out ch))
            (fail in out {:form form
                          :type ::unexpected-char
                          :actual ch}))
          (fail in out {:form form
                        :type ::unexpected-eof}))))))

(defn- -ws-char [form except]
  (let [except (set except)
        chs (into #{\space \backspace \formfeed \newline \return \tab} except)]
    (fn []
      (fn [in out ok fail]
        (if-let [ch (first in)]
          (if (contains? chs ch)
            (ok (rest in) (conj out ch))
            (fail in out {:form form
                          :type ::unexpected-char
                          :actual ch}))
          (fail in out {:form form
                        :type ::unexpected-eof}))))))

(defn- -end-of-string [form]
  (fn []
    (fn [in out ok fail]
      (if (empty? in)
        (ok in out)
        (fail in out {:form form
                      :type ::expected-eof})))))

(declare parser)

(defmulti vec->parser* (fn [_opts [type _props _children]] type))

(defmethod vec->parser* :default [{:keys [registry]} [ref _ _]]
  (fn []
    ((get @registry ref))))

(defmethod vec->parser* ::eps [_ [_ _ _]]
  (-eps))

(defmethod vec->parser* ::cat [opts [_ _ children :as form]]
  (let [children (mapv (partial parser opts) children)]
    (-cat form children)))

(defmethod vec->parser* ::alt [opts [_ _ children :as form]]
  (let [children (mapv (partial parser opts) children)]
    (-alt form children)))

(defmethod vec->parser* ::plus [opts [_ _ [child] :as form]]
  (let [child (parser opts child)]
    (-plus form child)))

(defmethod vec->parser* ::star [opts [_ _ [child]]]
  (let [child (parser opts child)]
    (-star child)))

(defmethod vec->parser* ::opt [opts [_ _ [child] :as form]]
  (let [child (parser opts child)]
    (-opt form child)))

(defmethod vec->parser* ::rep [opts [_ {:keys [times]} [child] :as form]]
  (let [child (parser opts child)]
    (-rep form times child)))

(defmethod vec->parser* ::la [opts [_ _ [child] :as form]]
  (let [child (parser opts child)]
    (-la form child)))

(defmethod vec->parser* ::nla [opts [_ _ [child] :as form]]
  (let [child (parser opts child)]
    (-neg-la form child)))

(defmethod vec->parser* ::skip [opts [_ _ [child] :as form]]
  (let [child (parser opts child)]
    (-skip form child)))

(defmethod vec->parser* ::keep [opts [_ _ [child] :as form]]
  (let [child (parser opts child)]
    (-keep form child)))

(defmethod vec->parser* ::char [_ [t props chs]]
  (-char (list* t props chs) chs))

(defmethod vec->parser* ::any-char [_ [_ {:keys [except]} _ :as form]]
  (-any-char form except))

(defmethod vec->parser* ::alpha [_ [_ {:keys [except]} _ :as form]]
  (-alpha-char form except))

(defmethod vec->parser* ::alpha-num [_ [_ {:keys [except]} _ :as form]]
  (-alpha-num-char form except))

(defmethod vec->parser* ::ws [_ [_ {:keys [except]} _ :as form]]
  (-ws-char form except))

(defmethod vec->parser* ::word [_ [_ _ [w] :as form]]
  (-word form w))

(defmethod vec->parser* ::eof [_ form]
  (-end-of-string form))

(defn- -wrap [P wrap-fn]
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
          children)
        form [t props children]]
    (cond-> (vec->parser* opts form)
      wrap (-wrap wrap)
      hide (->> (-skip form)))))

(defn keyword->parser [opts k]
  (vec->parser opts [k {}]))

(defn- default-ok [_in out]
  out)

(defn- default-fail [_in _out failure]
  failure)

(defn- -make-runnable [P]
  (fn f
    ([in] (f in [] default-ok default-fail))
    ([in out] (f in out default-ok default-fail))
    ([in out ok] (f in out ok default-fail))
    ([in out ok fail]
     (trampoline (P) in out ok fail))))

(defn parser
  ([form]
   (-make-runnable (parser {:registry (volatile! {})} form)))
  ([opts form]
   (cond
     (vector? form)
     (vec->parser opts form)

     (keyword? form)
     (keyword->parser opts form)

     (char? form)
     (vec->parser opts [::char form])

     (string? form)
     (vec->parser opts [::word form]))))

(defprotocol Grammar
  (-parser [this start-production-rule]))

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
           (-make-runnable P)
           (throw (ex-info "Unknown production rule" {:rule start-production-rule}))))))))
