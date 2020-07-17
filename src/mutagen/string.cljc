(ns mutagen.string
  (:require [mutagen.core :as m]
            #?@(:cljs [[clojure.string :as string]
                       [goog.string :as gstring]])))

(defprotocol Reader
  (peek-char [input] [this n])
  (read-char [input] [this n]))

(defprotocol IndexedReader
  (get-line-number [this])
  (get-column-number [this]))

#?(:cljs
   (declare ->StringInput))

(defrecord StringInput #?(:clj [^String st ^int pos]
                          :cljs [^string st ^number pos])
  Reader
  (peek-char [this]
    (nth st pos nil))
  (peek-char [this n]
    (when (<= (+ pos n) (count st))
      (reduce
       (fn [acc n]
         (if-let [ch (nth st (+ pos n) nil)]
           (conj acc ch)
           (reduced acc)))
       []
       (range n))))
  (read-char [this]
    (when-let [ch (peek-char this)]
      [ch (->StringInput st (inc pos))]))
  (read-char [this n]
    (when-let [chs (peek-char this n)]
      [chs (->StringInput st (+ pos (count chs)))])))

(defn -char [& chs]
  (let [chs (set chs)]
    (fn [in out ok fail]
      (let [ch (peek-char in)]
        (if (contains? chs ch)
          (let [[ch in] (read-char in)]
            (ok in (conj out ch)))
          (fail in out))))))

(defmethod m/vec->parser ::char [_ _ props chs]
  (let [form (into [::char props] chs)
        pf (apply -char chs)]
    (reify m/Parser
      (-form [_] form)
      (-parser [_] pf))))

(defn- -alpha-char []
  (fn [in out ok fail]
    (let [ch (peek-char in)]
      (if #?(:clj (Character/isLetter ch)
             :cljs (not= (string/lower-case ch)
                         (string/upper-case ch)))
        (let [[ch in] (read-char in)]
          (ok in (conj out ch)))
        (fail in out)))))

(defmethod m/vec->parser ::alpha-char [_ _ props & _]
  (let [form (into [::alpha-char props])
        pf (-alpha-char)]
    (reify m/Parser
      (-form [_] form)
      (-parser [_] pf))))

(defn- -alpha-num-char []
  (fn [in out ok fail]
    (let [ch (peek-char in)]
      (if #?(:clj (Character/isLetterOrDigit ch)
             :cljs (or (not= (string/lower-case ch)
                             (string/upper-case ch))
                       (gstring/isNumeric ch)))
        (let [[ch in] (read-char in)]
          (ok in (conj out ch)))
        (fail in out)))))

(defmethod m/vec->parser ::alpha-num-char [_ _ props & _]
  (let [form (into [::alpha-num-char props])
        pf (-alpha-num-char)]
    (reify m/Parser
      (-form [_] form)
      (-parser [_] pf))))

(defn -word [w]
  (let [w (seq w)
        size (count w)]
    (fn [in out ok fail]
      (if-let [[chs in'] (read-char in size)]
        (if (= w chs)
          (ok in' (conj out (apply str chs)))
          (fail in out))
        (fail in out)))))

(defmethod m/vec->parser ::word [_ _ props [w]]
  (let [form [::word props w]
        pf (-word w)]
    (reify m/Parser
      (-form [_] form)
      (-parser [_] pf))))

(defn -any-char [except]
  (let [except (set except)]
    (fn [in out ok fail]
      (if-let [[ch in'] (read-char in)]
        (if-not (contains? except ch)
          (ok in' (conj out ch))
          (fail in out))
        (fail in out)))))

(defmethod m/vec->parser ::any-char [_ _ {:keys [except] :as props} _]
  (let [form [::any-char props]
        pf (-any-char except)]
    (reify m/Parser
      (-form [_] form)
      (-parser [_] pf))))

(defn- -ws-char [in out ok fail]
  (if-let [ch (peek-char in)]
    (if (contains? #{\space \backspace \formfeed \newline \return \tab} ch)
      (ok (second (read-char in)) (conj out ch))
      (fail in out))
    (fail in out)))

(defmethod m/vec->parser ::ws [_ _ props _]
  (let [form [::ws props]
        pf -ws-char]
    (reify m/Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod m/vec->parser ::eof [& _]
  (let [form [::eof]
        pf (fn [{:keys [st pos] :as in} out ok fail]
             (if (>= pos (count st))
               (ok in out)
               (fail in out)))]
    (reify m/Parser
      (-form [_] form)
      (-parser [_] pf))))

(defn input [st]
  (->StringInput st 0))
