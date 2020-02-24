(ns mutagen.lexers.indexed-string
  (:require [mutagen.lexers.string :as s]))

(defn update-state [{:keys [line col] :as st} ch]
  (cond-> st
    true (-> (update :pos inc)
             (s/update-eof))
    (= \newline ch) (-> (update :line inc)
                        (assoc :col 0))
    (not= \newline ch) (update :col inc)
    (not (:sealed st)) (update :out conj {:ch ch :line line :col col})))

(defn char* [chs]
  (let [chs (set chs)]
    (fn [{:keys [pos in] :as st} ok fail]
      (let [ch (nth in pos :eof)]
        (cond
          (= :eof ch)
          (fail st :eof)

          (chs ch)
          (ok (update-state st ch))

          :else
          (fail st :unexpected-token))))))

(defn char1 [ch]
  (char* [ch]))

(defn any-char []
  (fn [{:keys [pos in] :as st} ok fail]
    (let [ch (nth in pos :eof)]
      (cond
        (= :eof ch)
        (fail st :eof)

        :else
        (ok (update-state st ch))))))

(defn word [w]
  (let [wc (count w)]
    (letfn [(check-word [st start]
              (try
                (= (subs st start (+ start wc)) w)
                (catch #?(:clj java.lang.StringIndexOutOfBoundsException
                          :cljs js/Error) _
                  :unexpected-token)))]
      (fn [{:keys [in pos] :as st} ok fail]
        (let [w' (check-word in pos)]
          (cond
            (keyword? w') (fail st w')
            w'            (ok (reduce #(update-state %1 %2) st w))
            (not w')      (fail st :unexpected-token)))))))

(defn state
  "Convert the string into internal state representation"
  [st]
  {:type :string
   :in st
   :pos 0
   :line 1
   :col 1
   :out []})

(defn parser [P]
  (letfn [(ok [st]
            {:result (:out st)
             :state (dissoc st :out)})
          (fail [_st failure]
            failure)]
    (fn [string]
      (let [st (state string)]
        (trampoline P st ok fail)))))
