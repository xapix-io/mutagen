(ns mutagen.grammar
  (:require [mutagen.combinators :as m]))

(defn normalize-chars [ch]
  (cond
    (and (sequential? ch)
         (= :range (first ch)))
    (m/char-range (second ch) (last ch))

    :else
    [ch]))

(def reserved-combinators #{:wrap :char :some-char :any-char :word :eof :eps :cat :alt :plus :star :rep :opt :neg :skip :lookahead :keep :resolve})

(defmacro emit [[t & args :as C]]
  (let [opts (if (map? (first args))
               (first args)
               {})
        args (if (map? (first args))
               (rest args)
               args)]
    (if (and ((some-fn :wrap-res :wrap-fail) opts)
             (not= :wrap t))
      `(emit [:wrap ~opts [~t ~(dissoc opts :wrap-res :wrap-fail) ~@args]])
      (case t
        :wrap      `(m/wrap (emit ~(first args)) ~opts)
        :char      `(m/char1 ~(first args))
        :some-char `(m/some-char ~@(mapcat normalize-chars args))
        :any-char  `(m/any-char)
        :word      `(m/word ~(first args))
        :eof       `(m/eof)
        :eps       `(m/eps)
        :cat       `(m/cat ~@args)
        :alt       `(m/alt ~@args)
        :plus      `(m/plus ~@args)
        :star      `(m/star ~@args)
        :rep       `(m/rep ~(first args) ~(second args))
        :opt       `(m/opt ~(first args))
        :neg       `(m/neg ~(first args))
        :skip      `(m/skip ~(first args))
        :lookahead `(m/lookahead ~(first args))
        :keep      `(m/keep ~(first args))
        :resolve   `(m/resolve ~(first args))

        t))))

(defn- emit* [parser]
  (cond
    (and (vector? parser)
         (not (= :range (first parser))))
    (list 'mutagen.grammar/emit (cons (first parser) (map emit* (rest parser))))

    :else parser))

(defn- replace-with-alias [bind-names parser]
  (cond
    (and (vector? parser)
         (= 1 (count parser))
         (not (reserved-combinators (first parser))))
    (get bind-names (first parser))

    (vector? parser)
    (into [(first parser)] (map (partial replace-with-alias bind-names) (rest parser)))

    :else
    parser))

(defn to-declare* [p]
  (lazy-seq
   (cond
     (and (vector? p)
          (= 1 (count p))
          (not (reserved-combinators (first p))))
     (list (first p))

     (vector? p)
     (mapcat to-declare* p)

     :else
     '())))

(defn to-declare [bind-map]
  (second (reduce
           (fn [[defined to-declare] [pn p]]
             (let [td (disj (set (to-declare* p)) pn)]
               ;; TODO Throw if combinator that needs to be resolved is not wrapperd into [:resolve ...]
               [(conj defined pn)
                (into to-declare (set (filter #(not (defined %)) td)))]))
           [#{} #{}]
           bind-map)))

(defmacro defgrammar [gname & bind-map]
  (let [bind-map (partition 2 bind-map)
        bind-names (->> bind-map
                        (map (fn [[n & _]]
                               (let [s (symbol (str (name n) "->" (name (gensym))))]
                                 [n s])))
                        (into {}))
        bindings (->> bind-map
                      (map (fn [[bind-name bind-parser]]
                             [(get bind-names bind-name)
                              (emit* (replace-with-alias bind-names bind-parser))])))
        to-declare (to-declare bind-map)]
    `(do
       (declare ~@(map bind-names to-declare))
       ~@(map (fn [[bind-name bind-parser]]
                `(def ^:private ~bind-name ~bind-parser))
              bindings)
       (defn ~gname [start-production#]
         (m/parser (get ~bind-names start-production#))))))

(comment

  (defgrammar XS
    :X [:char \a]
    :XS [:cat {:wrap-res (fn [xs] [(apply str (map :ch xs))])}
         [:char \(]
         [:star [:cat [:X] [:char \,]]]
         [:alt
          [:cat [:X] [:char \)]]
          [:char \)]]])

  ((XS :XS) "(a,a,a)")

  )
