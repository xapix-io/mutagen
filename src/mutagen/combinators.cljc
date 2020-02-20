(ns mutagen.combinators
  (:refer-clojure :exclude [cat])
  (:require [mutagen.failure :as failure]
            [mutagen.state :as state]))

(defn cat [& parsers]
  (if (seq (rest parsers))
    (let [parser (first parsers)
          parsers (apply cat (rest parsers))]
      (fn [st ok fail]
        (letfn [(ok' [st]
                  (fn []
                    (parsers st ok fail)))]
          (fn []
            (parser st ok' fail)))))
    (let [parser (first parsers)]
      (fn [st ok fail]
        (fn []
          (parser st ok fail))))))

(defn alt* [parsers]
  (if (seq (rest parsers))
    (let [p (first parsers)
          ps (alt* (rest parsers))]
      (fn [st ok fail failures]
        (fn []
          (p st ok (fn [_st failure]
                     (ps st ok fail (conj failures failure)))))))
    (fn [st ok fail failures]
      ((first parsers) st ok (fn [st failure]
                               (fail st (conj failures failure)))))))

(defn alt [& parsers]
  (let [alt-parser (alt* parsers)]
    (fn [st ok fail]
      (fn []
        (alt-parser st ok (fn [_st failures]
                            (fail st (failure/alt st failures)))
                    [])))))

(defn star [parser]
  (fn star* [st ok fail]
    (fn []
      (letfn [(ok' [st]
                (if (state/eof? st)
                  (ok st)
                  (star* st ok fail)))
              (fail' [_st _failure]
                (ok st))]
        (parser st ok' fail')))))

(defn plus [parser]
  (fn [st ok fail]
    (fn []
      (letfn [(ok' [st]
                (let [p (star parser)]
                  (fn []
                    (p st ok fail))))]
        (parser st ok' fail)))))

(defn rep
  ([n parser] (rep n 1 parser))
  ([N n parser]
   (fn [st ok fail]
     (fn []
       (letfn [(ok' [st]
                 (if (= n N)
                   (ok st)
                   ((rep N (inc n) parser) st ok fail)))]
         (parser st ok' fail))))))

(defn opt [parser]
  (fn [st ok _fail]
    (fn []
      (letfn [(fail' [_ _]
                (ok st))]
        (parser st ok fail')))))

(defn neg [parser]
  (fn [st ok fail]
    (letfn [(ok' [_st]
              (fail st (failure/unexpected-success st)))
            (fail' [st _failure]
              (ok st))]
      (fn []
        (parser st ok' fail')))))

(defn eps []
  (fn [st ok _fail]
    (fn []
      (ok st))))

(defn eof []
  (fn [st ok fail]
    (fn []
      (if (state/eof? st)
        (ok st)
        (fail st (failure/unexpected-eof st))))))

(defn wrap
  ([p ok-wrapper]
   (wrap p ok-wrapper (fn [_st failure] failure)))
  ([p ok-wrapper fail-wrapper]
   (fn [{:keys [out] :as st} ok fail]
     (letfn [(ok' [st']
               (let [out' (ok-wrapper (:out st') (partial fail st'))]
                 (if (fn? out')
                   out'
                   (ok (assoc st' :out (into out out'))))))
             (fail' [st' failure]
               (fail st' (fail-wrapper st' failure)))]
       (fn []
         (p (assoc st :out []) ok' fail'))))))

(defn discard [parser]
  (fn [{:keys [out] :as st} ok fail]
    (fn []
      (parser st (fn [st]
                   (ok (assoc st :out out))) fail))))

(defn lookforward [check parser]
  (fn [st ok fail]
    (fn []
      (check st (fn [_]
                  (parser st ok fail))
             fail))))
