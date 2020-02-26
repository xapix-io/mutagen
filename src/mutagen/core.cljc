(ns mutagen.core
  (:refer-clojure :exclude [cat resolve]))

(defn char-range [start end]
  (map char
       (range #?(:clj (int start) :cljs (.charCodeAt start 0))
              (inc #?(:clj (int end) :cljs (.charCodeAt end 0))))))

(defmacro resolve [p]
  (let [st (gensym)
        ok (gensym)
        fail (gensym)]
    `(fn [~st ~ok ~fail]
       (~p ~st ~ok ~fail))))

(defn consume-char [{:keys [line col sealed] :as st} ch]
  (cond-> st
    true               (update :pos inc)
    (not sealed)       (update :out #(conj % {:ch ch :line line :col col}))
    (= \newline ch)    (-> (update :line inc)
                           (assoc :col 0))
    (not= \newline ch) (update :col inc)))

(defn char1 [ch]
  (fn [{:keys [in pos] :as st} ok fail]
    (let [ch' (nth in pos ::eof)]
      (cond
        (= ::eof ch')
        (fail st {:type     ::unexpected-eof
                  :expected ch})

        (= ch ch')
        (ok (consume-char st ch))

        :else
        (fail st {:type     ::unexpected-token
                  :expected ch
                  :actual   ch'})))))

(defn some-char [& chs]
  (let [chs' (set chs)]
    (fn [{:keys [in pos] :as st} ok fail]
      (let [ch' (nth in pos ::eof)]
        (cond
          (= ::eof ch')
          (fail st {:type            ::unexpected-eof
                    :expected-one-of chs'})

          (chs' ch')
          (ok (consume-char st ch'))

          :else
          (fail st {:type            ::unexpected-token
                    :expected-one-of chs'
                    :actual          ch'}))))))

(def any-char
  (fn [{:keys [pos in] :as st} ok fail]
    (let [ch (nth in pos ::eof)]
      (cond
        (= ::eof ch)
        (fail st {:type ::unexpected-eof})

        :else
        (ok (consume-char st ch))))))

(defn word [w]
  (letfn [(get-word [pos in]
            (if (= pos (count in))
              ::eof
              (try
                (subs in pos (+ pos (count w)))
                (catch #?(:clj java.lang.StringIndexOutOfBoundsException
                          :cljs js/Error) _
                  [(subs in pos (count in)) ::eof]))))]
    (fn [{:keys [pos in] :as st} ok fail]
      (let [w' (get-word pos in)]
        (cond
          (= ::eof w') (fail st {:type ::unexpected-eof
                                 :expected w})
          (vector? w') (fail st {:type ::unexpected-eof
                                 :expected w
                                 :actual w'})
          (= w w')     (ok (reduce consume-char st w))

          :else        (fail st {:type ::unexpected-token
                                 :expected w
                                 :actual w'}))))))

(def eof
  (fn [{:keys [pos in] :as st} ok fail]
    (if (= pos (count in))
      (ok st)
      (fail st {:type ::unexpected-token
                :expected ::eof
                :actual (nth in pos)}))))

(def eps
  (fn [st ok _fail]
    (ok st)))

(defn cat
  ([P Q]
   (fn [st ok fail]
     (fn []
       (P st
          (fn [st]
            (fn []
              (Q st ok fail)))
          fail))))
  ([P Q & PS]
   (reduce cat (list* P Q PS))))

(defn alt
  ([P Q]
   (fn [st ok fail]
     (fn []
       (P st ok (fn [_st' failure']
                  (fn []
                    (Q st ok (fn [_st'' failure'']
                               (let [failures (if (sequential? failure')
                                                (cons failure'' failure')
                                                (list failure'' failure'))]
                                 (fail st failures))))))))))
  ([P Q & PS]
   (let [ALT (reduce alt (list* P Q PS))]
     (fn [st ok fail]
       (ALT st ok (fn [st failures]
                    (fail st {:type ::alt
                              :failures failures})))))))



(defn star [P]
  (fn star* [{:keys [pos in] :as st} ok fail]
    (fn []
      (if (= pos (count in))
        (ok st)
        (P st (fn [st] (fn [] (star* st ok fail)))
           (fn [_ _]
             (ok st)))))))

(defn plus [P]
  (let [P* (star P)]
    (fn [st ok fail]
      (fn []
        (P st (fn [st]
                (fn [] (P* st ok fail)))
           fail)))))

(defn rep [N P]
  (let [PS (map (fn [i]
                  (fn [st ok fail]
                    (fn []
                      (P st ok (fn [st failure]
                                 (fail st {:type ::times
                                           :expected N
                                           :actual i
                                           :cause failure}))))))
                (range N))]
    (apply cat PS)))

(defn opt [P]
  (fn [st ok _fail]
    (fn []
      (P st ok (fn [_ _]
                 (ok st))))))

(defn neg [P]
  (fn [st ok fail]
    (fn []
      (P st (fn [st]
              (fail st {:type ::neg}))
         (fn [_ _]
           (ok st))))))

(defn skip [P]
  (fn [st ok fail]
    (fn []
      (P (assoc st :sealed true)
         (fn [st]
           (ok (dissoc st :sealed)))
         (fn [st failure]
           (fail (dissoc st :sealed) failure))))))

(defn wrap [P {:keys [wrap-res wrap-fail]}]
  (fn [{:keys [out sealed] :as st} ok fail]
    (if sealed
      (P st ok fail)
      (P (assoc st :out [])
         (if wrap-res
           (fn [st']
             (ok (update st' :out #(into out (wrap-res %)))))
           ok)
         (if wrap-fail
           (fn [st' failure]
             (fail st' (wrap-fail failure {:prev-ok st
                                           :st st'})))
           fail)))))

(defn lookahead [P]
  (fn [st ok fail]
    (fn []
      (P st (fn [_]
              (ok st))
         fail))))

(defn parser [P]
  (fn [string ok fail]
    (trampoline P {:in string
                   :pos 0
                   :line 1
                   :col 1
                   :out []}
                ok fail)))
