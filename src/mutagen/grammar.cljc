(ns mutagen.grammar
  (:require [mutagen.combinators :as comb]
            [clojure.walk :refer [prewalk postwalk]]))

(defmulti parser* (fn [_ [t opts & _]]
                    (if (or (:ok-wrapper opts)
                            (:fail-wrapper opts))
                      :wrap
                      t)))

(defmethod parser* :cat [m [_ _ & ps]]
  (let [ps' (map (partial parser* m) ps)]
    (fn []
      (apply comb/cat (map #(%) ps')))))

(defmethod parser* :alt [m [_ _ & ps]]
  (let [ps (map (partial parser* m) ps)]
    (fn []
      (apply comb/alt (map #(%) ps)))))

(defmethod parser* :star [m [_ _ p]]
  (let [p (parser* m p)]
    (fn []
      (comb/star (p)))))

(defmethod parser* :plus [m [_ _ p]]
  (let [p (parser* m p)]
    (fn []
      (comb/plus (p)))))

(defmethod parser* :rep [m [_ _ n p]]
  (let [p (parser* m p)]
    (fn []
      (comb/rep n (p)))))

(defmethod parser* :opt [m [_ _ p]]
  (let [p (parser* m p)]
    (fn []
      (comb/opt (p)))))

(defmethod parser* :neg [m [_ _ p]]
  (let [p (parser* m p)]
    (fn []
      (comb/neg (p)))))

(defmethod parser* :discard [m [_ _ p]]
  (let [p (parser* m p)]
    (fn []
      (comb/discard (p)))))

(defmethod parser* :wrap [m [_ {:keys [ok-wrapper fail-wrapper]
                                :or {ok-wrapper identity
                                     fail-wrapper (fn [_st failure] failure)}} & _ :as p]]
  (let [p (parser* m (update p 1 #(dissoc % :ok-wrapper :fail-wrapper)))]
    (fn [& args]
      (comb/wrap (apply p args) ok-wrapper fail-wrapper))))

(defmethod parser* :eof [_ _]
  (fn []
    (comb/eof)))

(defmethod parser* :eps [_ _]
  (fn []
    (comb/eps)))

(defmethod parser* :default [m [p opts & args]]
  (let [opts (dissoc opts :name :args)]
    (when (string? p)
      (cond
        (seq args)
        (fn []
          (fn [st ok fail]
            (let [p ((parser* m (update (apply (get m p) args) 1 #(merge % opts))))]
              (fn []
                (p st ok fail)))))
        :else
        (fn [& args]
          (fn [st ok fail]
            (let [p ((parser* m (update (apply (get m p) args) 1 #(merge % opts))))]
              (fn []
                (p st ok fail)))))))))

(defn make-parser-fn [[_ {:keys [args] :as m} :as f]]
  (fn [& args']
    (assert (= (count args) (count args')) (str "Wrong number of arguments passed to parser " f))
    (let [m (dissoc m :name :args)
          f (into [(first f) m] (drop 2 f))
          argsm (zipmap args args')]
      (prewalk
       (fn [x]
         (cond
           (map-entry? x)
           x

           (and (sequential? x)
                ((some-fn keyword? string?) (first x)))
           (let [pt (first x)
                 m (if (map? (second x))
                     (dissoc (second x) :name :args)
                     {})
                 pargs (if (map? (second x))
                         (drop 2 x)
                         (rest x))]
             (into [pt m] (map (fn [x] (if (symbol? x) (get argsm x) x)) pargs)))

           :else
           x))
       f))))

(defn find-named-parsers [f]
  (let [m (atom {})]
    (postwalk
     (fn [x]
       (if (and (sequential? x)
                (map? (second x))
                (contains? (second x) :name))
         (do
           (swap! m #(assoc % (get-in x [1 :name]) (make-parser-fn x)))
           x)
         x))
     f)
    @m))

(defn normalize-parser-opts [f]
  (postwalk
   (fn [x]
     (cond
       (map-entry? x) x
       (and (sequential? x)
            ((some-fn string? keyword?) (first x)))
       (let [pt (first x)
             m (if (map? (second x))
                 (second x)
                 {})
             pargs (if (map? (second x))
                     (drop 2 x)
                     (rest x))]
         (into [pt m] pargs))
       :else x))
   f))

(defn normalize-named [f]
  (postwalk
   (fn [x]
     (cond
       (map-entry? x) x
       (and (sequential? x)
            ((some-fn string? keyword?) (first x)))
       (if (:name (second x))
         [(:name (second x))]
         x)

       :else x))
   f))

(defn grammar [g]
  (reduce-kv
   (fn [acc term p]
     (let [np (normalize-parser-opts p)]
       (merge acc (find-named-parsers (assoc-in np [1 :name] term)))))
   {}
   g))

(defn compile-parser [grammar p]
  (let [np (normalize-parser-opts p)
        p (parser* (merge grammar (find-named-parsers p))
                   (normalize-named np))]
    (fn [& args]
      (apply p args))))
