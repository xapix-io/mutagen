(ns mutagen.combinators
  (:refer-clojure :exclude [cat keep trampoline])
  #?(:cljs (:require [clojure.string :as string]
                     [goog.string :as gstring])))

(defrecord Consumed [state ok])
(defrecord Begin [parser state ok fail])

(defn- ->set
  ([chs] (->set #{} chs))
  ([init-set chs]
   (reduce
    (fn [s ch]
      (if (or (sequential? ch)
              (string? ch))
        (->set s ch)
        (conj s ch)))
    init-set
    chs)))

(defn char-range [start end]
  (map char
       (range #?(:clj (int start) :cljs (.charCodeAt start 0))
              (inc #?(:clj (int end) :cljs (.charCodeAt end 0))))))

(defmacro resolve-combinator [p]
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

;; === Low level combinators ===

(defn examine-char [f & {:keys [expected]}]
  (fn [{:keys [in pos line col] :as st} ok fail]
    (let [ch' (nth in pos ::eof)]
      (cond
        (= ::eof ch')
        (fail st (merge
                  {:type     ::unexpected-eof
                   :at [line col]}
                  (when expected
                    {:expected expected})))

        (f ch')
        (let [state (consume-char st ch')]
          (->Consumed state ok))

        :else
        (fail st (merge
                  {:type     ::unexpected-token
                   :expected expected
                   :actual   ch'
                   :at [line col]}
                  (when expected
                    {:expected expected})))))))

(defn char1
  "Creates a function of state, ok callback and fail callback.
  This function should consume one character from the position
  if the character on that position is identical to `ch`.
  Continue execution by creating `Consumed` state if successfull
  and directly call `fail` callback overwise."
  [ch]
  (examine-char #(= % ch) :expected ch))

(defn some-char
  "Same as `char1` but will create `Consumed` state when 'to-consume'
  character is from the `chs` set."
  [& chs]
  (let [chs' (->set chs)]
    (fn [{:keys [in pos line col] :as st} ok fail]
      (let [ch' (nth in pos ::eof)]
        (cond
          (= ::eof ch')
          (fail st {:type            ::unexpected-eof
                    :expected-one-of chs'
                    :at [line col]})

          (chs' ch')
          (let [state (consume-char st ch')]
            (->Consumed state ok))

          :else
          (fail st {:type            ::unexpected-token
                    :expected-one-of chs'
                    :actual          ch'
                    :at [line col]}))))))

(def alpha-char
  (examine-char #?(:clj #(Character/isLetter %)
                   :cljs #(not= (string/lower-case %)
                                (string/upper-case %)))
                :expected :alpha-character))

(def alpha-numeric
  (examine-char #?(:clj #(Character/isLetterOrDigit %)
                   :cljs #(or (not= (string/lower-case %)
                                    (string/upper-case %))
                              (gstring/isNumeric %)))
                :expected :alpha-numeric-character))

(defn any-char []
  (fn [{:keys [pos in line col] :as st} ok fail]
    (let [ch (nth in pos ::eof)]
      (cond
        (= ::eof ch)
        (fail st {:type ::unexpected-eof
                  :at [line col]})

        :else
        (let [state (consume-char st ch)]
          (->Consumed state ok))))))

(declare cat neg)

(defn any-char-except [& chs]
  (cat (neg (apply some-char chs))
       (any-char)))

(defn word [w]
  (if (= 1 (count w))
    (char1 (first w))
    (apply cat (map char1 w))))

;; === High level combinators ===

(defn cat
  ([P] P)
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

(defn eof []
  (fn [{:keys [pos in line col] :as st} ok fail]
    (if (= pos (count in))
      (ok st)
      (fail st {:type ::unexpected-token
                :expected ::eof
                :actual (nth in pos)
                :at [line col]}))))

(defn eps []
  (fn [st ok _fail]
    (ok st)))

(defn alt
  ([P] P)
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
                              :failures failures
                              :at ((juxt :line :col) st)})))))))



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
                                           :cause failure
                                           :at ((juxt :line :col) st)}))))))
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
              (fail st {:type ::neg
                        :at ((juxt :line :col) st)}))
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

(defn wrap
  ([P] (wrap P nil nil))
  ([P wrap-res]
   (if (map? wrap-res)
     (wrap P (:wrap-res wrap-res) (:wrap-fail wrap-res))
     (wrap P wrap-res nil)))
  ([P wrap-res wrap-fail]
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
            fail))))))

(defn lookahead [P]
  (fn [st ok fail]
    (fn []
      (P st (fn [_]
              (ok st))
         fail))))

(defn keep [P]
  (fn [st ok fail]
    (fn []
      (P st (fn [st']
              (ok (assoc st :out (:out st'))))
         fail))))

(def ws-char
  (some-char
   \space
   \backspace
   \formfeed
   \newline
   \return
   \tab))

(def skip-ws
  (skip (star ws-char)))

(def ^:dynamic *keep-consumed?* false)
(def ^:dynamic *intermediate-states*)

(defn trampoline
  ([f]
   (let [ret (f)]
     (cond
       (fn? ret)
       (recur ret)

       (instance? Begin ret)
       (let [{:keys [state parser ok fail]} ret]
         (when *keep-consumed?*
           (swap! *intermediate-states* assoc 0 [{:state state
                                                  :parser parser
                                                  :ok ok
                                                  :fail fail}]))
         (recur #(parser state ok fail)))

       (instance? Consumed ret)
       (let [{:keys [ok state]} ret]
         ;; (prn "---TICK" (:pos state))
         (when *keep-consumed?*
           (let [{:keys [pos]} state]
             (swap! *intermediate-states* update pos (fnil conj []) {:state (dissoc state :in)
                                                                     :ok ok})))
         (recur #(ok state)))

       :else
       ret)))
  ([f state ok fail]
   (trampoline #(->Begin f state ok fail))))

(defn parser [P]
  (fn p
    ([string] (p string
                 (fn [{:keys [out in pos]}]
                   (if-let [tail (not-empty (subs in pos))]
                     (conj out tail) out))
                 (fn [st failure]
                   ;; TODO proper failure format needed, add pointer to original source + failure message from the parser
                   (throw (ex-info "Failure!" {:state st
                                               :failure failure})))))
    ([string ok fail]
     (trampoline P {:in string
                    :pos 0
                    :line 1
                    :col 1
                    :out []}
                 ok fail))))

(defn find-common-index [string-1 string-2]
  (let [pos (or (first (keep-indexed (fn [i [ch1 ch2]]
                                       (when (not= ch1 ch2) (dec i)))
                                     (map vector string-2 string-1)))
                (dec (count string-2)))]
    (if (< pos 0) 0 pos)))

(defn invalidate-states
  ([states]
   (reduce-kv
    (fn [acc [pos states]]
      (if (> (count states) 1)
        acc
        (assoc acc pos states)))
    {}
    states))
  ([states new-string]
   (let [old-string (get-in states [0 0 :state :in])
         pos (find-common-index new-string old-string)]
     (reduce-kv
      (fn [acc pos' states]
        (if (or (> pos' pos) (> (count states) 1))
          acc
          (assoc acc pos' states)))
      {}
      (assoc-in states [0 0 :state :in] new-string)))))

(defn shallow-parser
  ([P] (shallow-parser P {}))
  ([P states]
   (fn
     ([string]
      (let [states (invalidate-states states string)]
        ;; (prn "---STATE" (get states 0))
        (binding [*keep-consumed?* true
                  *intermediate-states* (atom states)]
          (let [pos (apply max (keys states))
                {:keys [ok state fail parser]} (first (get states (or pos 0)))]
            (cond
              (and fail parser ok state)
              (trampoline P (assoc state :in string) ok fail)

              (and ok state)
              (trampoline #(ok (assoc state :in string)))

              :else
              (throw (ex-info "Can not find intermediate state" {})))
            (let [states @*intermediate-states*]
              (shallow-parser P states))))))
     ([string ok fail]
      (binding [*keep-consumed?* true
                *intermediate-states* (atom {})]
        (trampoline P {:in string
                       :pos 0
                       :line 1
                       :col 1
                       :out []}
                    ok fail)
        (let [states @*intermediate-states*]
          (shallow-parser P states)))))))

(comment

  (let [P (cat (word "oof")
               (char1 \space)
               (alt (cat (word "foo") (char1 \space) (alt (word "bar")
                                                          (word "bur")))
                    (cat (word "foo") (char1 \space) (word "baz"))))]
    (-> (shallow-parser P)
        (.invoke "oof " prn prn)
        (.invoke "oof fo")
        (.invoke "oof foo bar")
        (.invoke "oof foo bur")))

  )
