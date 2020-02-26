(ns mutagen.grammar
  (:require [mutagen.combinators :as m]
            [clojure.walk :refer [postwalk prewalk]]))

(defn normalize-chars [ch]
  (cond
    (and (sequential? ch)
         (= :range (first ch)))
    (m/char-range (second ch) (last ch))

    :else
    [ch]))

(declare emit)

(defmacro emit-char [_opts ch]
  `(m/char1 ~ch))

(defmacro emit-wrap [opts P]
  `(m/wrap (emit ~P) ~opts))

(defmacro emit-some-char [_opts & chs]
  `(m/some-char ~@(mapcat normalize-chars chs)))

(defmacro emit-word [_opts word]
  `(m/word ~word))

(defmacro emit-cat [_opts & PS]
  `(m/cat ~@PS))

(defmacro emit-alt [_opts & PS]
  `(m/alt ~@PS))

(defmacro emit-star [_opts P]
  `(m/star ~P))

(defmacro emit-plus [_opts P]
  `(m/plus ~P))

(defmacro emit-rep [_opts N P]
  `(m/rep ~N ~P))

(defmacro emit-opt [_opts P]
  `(m/opt ~P))

(defmacro emit-neg [_opts P]
  `(m/neg ~P))

(defmacro emit-skip [_opts P]
  `(m/skip ~P))

(defmacro emit-lookahead [_opts P]
  `(m/lookahead ~P))

(defmacro emit-keep [_opts P]
  `(m/keep ~P))

(defmacro emit-resolve [_opts to-resolve & _args]
  `(m/resolve ~to-resolve))

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
        :wrap      `(emit-wrap ~opts ~@args)
        :char      `(emit-char ~opts ~@args)
        :some-char `(emit-some-char ~opts ~@args)
        :any-char  `m/any-char
        :word      `(emit-word ~opts ~@args)
        :eof       `m/eof
        :eps       `m/eps
        :cat       `(emit-cat ~opts ~@args)
        :alt       `(emit-alt ~opts ~@args)
        :plus      `(emit-plus ~opts ~@args)
        :star      `(emit-star ~opts ~@args)
        :rep       `(emit-rep ~opts ~@args)
        :opt       `(emit-opt ~opts ~@args)
        :neg       `(emit-neg ~opts ~@args)
        :skip      `(emit-skip ~opts ~@args)
        :lookahead `(emit-lookahead ~opts ~@args)
        :keep      `(emit-keep ~opts ~@args)

        (if (keyword? t)
          `(emit-resolve ~opts ~(symbol (name t)) ~@args)
          (throw (ex-info "Unknown combinator" {:combinator C})))))))

(defmacro recursive-emit [P]
  (postwalk
   (fn [x]
     (if (and (vector? x)
              (not (map-entry? x))
              (not (and (= 3 (count x))
                        (= :range (first x))
                        (char? (second x))
                        (char? (last x)))))
       (list 'mutagen.grammar/emit x)
       x))
   P))

(defmacro defgrammar [bindings]
  (let [helpers (atom {})
        bindings (->> bindings
                      (map (fn [[bind-name bind-parser]]
                             [(symbol (name bind-name))
                              (prewalk
                               (fn [x]
                                 (if (and (list? x) (= 'fn (first x)))
                                   (if-let [fn-s (get @helpers x)]
                                     fn-s
                                     (let [fn-s (gensym)]
                                       (swap! helpers assoc x fn-s)
                                       fn-s))
                                   x))
                               bind-parser)]))
                      (into {}))]
    `(do
       (declare ~@(keys bindings))
       ~@(for [[helper-body helper-name] @helpers]
           `(def ~helper-name ~helper-body))
       ~@(for [[bind-name parser] bindings]
           `(def ~bind-name (recursive-emit ~parser))))))

(comment

  (defgrammar
    {:A [:char {:wrap (fn [xs] xs)} \a]
     :B [:char {:wrap (fn [xs] xs)} \b]})

  )
