(ns mutagen.parsers.string
  (:require [mutagen.failure :as failure]
            [mutagen.state :as state]
            [mutagen.grammar :as grammar]))

(defn char-range [start end]
  (map char (range #?(:clj (int start) :cljs (.charCodeAt start 0))
                   (inc #?(:clj (int end) :cljs (.charCodeAt end 0))))))

(defn update-state [ch {:keys [line col] :as st}]
  (cond-> st
    (= \newline ch) (-> (update :line inc)
                        (assoc :col 0))
    (not= \newline ch) (update :col inc)
    true (update :pos inc)
    true (update :out conj {:ch ch :line line :col col})))

(defn some-char [chs]
  (fn [{in :in pos :pos :as st} ok fail]
    (let [ch' (nth in pos :eof)]
      (fn []
        (cond
          (= :eof ch')
          (fail st (failure/unexpected-eof st))

          (chs ch')
          (ok (update-state ch' st))

          :else
          (fail st (failure/unexpected-token st ch')))))))

(defn any-char []
  (some-char (constantly true)))

(defn single-char [ch]
  (some-char (set [ch])))

(defn word [w]
  (fn [{:keys [in pos] :as st} ok fail]
    (fn []
      (let [w' (try
                 (subs in pos (+ pos (count w)))
                 (catch #?(:clj java.lang.StringIndexOutOfBoundsException
                           :cljs js/Error) _
                   (failure/unexpected-eof st)))]
        (cond
          (failure/fail? w')
          (fail st w')

          (= w' w)
          (ok (reduce
               (fn [st ch]
                 (update-state ch st))
               st w))

          :else
          (fail st (failure/unexpected-token st w')))))))

(defn char* [ch]
  (cond
    (char? ch)
    [ch]

    (and (sequential? ch)
         (= :range (first ch))
         (= 4 (count ch)))
    (apply char-range (drop 2 ch))

    (and (sequential? ch)
         (every? char? ch))
    ch))

(defmethod state/eof? :string [{:keys [pos in]}]
  (>= pos (count in)))

(defmethod grammar/parser* :some-char [_ [_ _ & chs]]
  (fn []
    (some-char (set (mapcat char* chs)))))

(defmethod grammar/parser* :any-char [_ _]
  (fn []
    (any-char)))

(defmethod grammar/parser* :char [_ [_ _ ch]]
  (fn []
    (single-char ch)))

(defmethod grammar/parser* :word [_ [_ _ w]]
  (fn []
    (word w)))

(defn state [st]
  {:type :string
   :in st
   :pos 0
   :line 1
   :col 1
   :out []})

(defn parser
  ([G P] (parser (grammar/compile-parser (grammar/grammar G) P)))
  ([P]
   (letfn [(ok [st]
             {:result (:out st)
              :state st})
           (fail [_st failure]
             failure)]
     (fn [string & args]
       (let [st (state string)]
         (trampoline (apply P args) st ok fail))))))
