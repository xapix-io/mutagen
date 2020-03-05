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

(defmacro emit-wrap [NS opts P]
  `(m/wrap (emit ~NS ~P) ~opts))

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

(defmacro emit [NS [t & args :as C]]
  (let [opts (if (map? (first args))
               (first args)
               {})
        args (if (map? (first args))
               (rest args)
               args)]
    (if (and ((some-fn :wrap-res :wrap-fail) opts)
             (not= :wrap t))
      `(emit ~NS [:wrap ~opts [~t ~(dissoc opts :wrap-res :wrap-fail) ~@args]])
      (case t
        :wrap      `(emit-wrap ~NS ~opts ~@args)
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
        :resolve   `(emit-resolve ~opts ~@args)

        (if (keyword? t)
          (symbol (str NS "->" (name t)))
          (throw (ex-info "Unknown combinator" {:combinator C})))))))

(defmacro recursive-emit [NS P]
  (postwalk
   (fn [x]
     (if (and (vector? x)
              (not (map-entry? x))
              (not (and (= 3 (count x))
                        (= :range (first x))
                        (char? (second x))
                        (char? (last x)))))
       (list 'mutagen.grammar/emit NS x)
       x))
   P))

(defmacro defgrammar [gname & bindings]
  (let [grammar-ns (name gname)
        helpers (atom {})
        to-declare (atom #{})
        ;; Skip docstring if present
        bindings (if (string? (first bindings))
                   (rest bindings)
                   bindings)
        bindings (->> (partition 2 bindings)
                      (map (fn [[bind-name bind-parser]]
                             (let [bind-name (symbol (str grammar-ns "->" (name bind-name)))]
                               [bind-name
                                (prewalk
                                 (fn [x]
                                   (cond
                                     (and (map-entry? x)
                                          (#{:wrap-res :wrap-fail} (first x)))
                                     (if-let [fn-s (get @helpers (second x))]
                                       [(first x) (symbol (name fn-s))]
                                       (let [fn-s (gensym)]
                                         (swap! helpers assoc (second x) fn-s)
                                         [(first x) (symbol (name fn-s))]))

                                     (and (vector? x)
                                          (= :resolve (first x)))
                                     (do
                                       (swap! to-declare conj (symbol (str grammar-ns "->" (-> x second first name))))
                                       x)

                                     :else
                                     x))
                                 bind-parser)])))
                      (into []))]
    `(do
       ~@(for [[helper-body helper-name] @helpers]
           `(def ~helper-name ~helper-body))
       (declare ~@(seq @to-declare))
       ~@(for [[bind-name parser] bindings]
           `(def ~bind-name (recursive-emit ~grammar-ns ~parser))))))

(defn- start-production-symbol [grammar start]
  (symbol (str (name grammar) "->" (name start))))

(defmacro defparser [pname grammar start]
  (let [start-production (start-production-symbol grammar start)]
    `(def ~pname (m/parser ~start-production))))

(defmacro defparser* [pname grammar start]
  (let [start-production (start-production-symbol grammar start)]
    `(def ~pname (m/shallow-parser ~start-production))))
