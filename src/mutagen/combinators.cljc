(ns mutagen.combinators
  (:refer-clojure :exclude [cat]))

(defn cat [& PS]
  (if (= 2 (count PS))
    (let [[P P1] PS]
      (fn [st ok fail]
        (fn []
          (P st (fn [st] (P1 st ok fail)) fail))))
    (let [[P P1] (take-last 2 PS)
          PS (drop-last 2 PS)]
      (apply cat (concat PS (list (fn [st ok fail]
                                    (fn []
                                      (P st (fn [st] (P1 st ok fail)) fail)))))))))

(defn alt* [PS]
  (if (= 2 (count PS))
    (let [[P P1] PS]
      (fn [st ok fail]
        (fn []
          (P st ok (fn [st' failure]
                     (fn []
                       (P1 st [{:fail failure
                                :state st'}]
                           ok fail)))))))
    (let [[P P1] (take-last 2 PS)
          PS (vec (drop-last 2 PS))]
      (alt* (conj PS (fn [st failures ok fail]
                       (fn []
                         (P st ok
                            (fn [st' failure']
                              (fn []
                                (P1 st failures ok
                                    (fn [st'' failure'']
                                      (fail st (into failures [{:fail failure'
                                                                :state st'}
                                                               {:fail failure''
                                                                :state st''}]))))))))))))))

(defn alt [& PS]
  (alt* (conj (vec PS) (fn [st failures _ok fail]
                         (fail st failures)))))

(defn star [P]
  (fn star-fn [st ok _fail]
    (if (:eof? st)
      (ok st)
      (fn []
        (P st
           (fn [st]
             (star-fn st ok _fail))
           (fn [_ _] (ok st)))))))

(defn plus [P]
  (letfn [(P' [ok]
            (fn K [st]
              (if (:eof? st)
                (ok st)
                (P st K (fn [_ _] (ok st))))))]
    (fn [st ok fail]
      (P st (P' ok) fail))))

(defn rep [N P]
  (apply cat (map-indexed (fn [n P]
                            (fn [st ok fail]
                              (P st ok (fn [st failure]
                                         (fail st {:fail :rep
                                                   :at (inc n)
                                                   :failure failure})))))
                          (repeat N P))))

(defn opt [P]
  (fn [st ok _fail]
    (P st ok (fn [_ _] (ok st)))))

(defn neg [P]
  (fn [st ok fail]
    (P st
       (fn [_st]
         (fail st {:fail :unexpected-success}))
       (fn [_st _]
         (ok st)))))

(defn wrap [P {:keys [ok-wrapper fail-wrapper]}]
  (fn [{:keys [out sealed] :as st} ok fail]
    (if sealed
      (P st ok fail)
      (P (assoc st :out [])
       (if ok-wrapper
         (fn [st']
           (ok (update st' :out #(into out (ok-wrapper %)))))
         ok)
       (if fail-wrapper
         (fn [st' failure]
           (fail st' (fail-wrapper st' failure)))
         fail)))))

(defn discard [P]
  (fn [st ok fail]
    (P (assoc st :sealed true)
     (fn [st]
       (ok (dissoc st :sealed)))
     fail)))

(defn eof []
  (fn [st ok fail]
    (if (:eof? st)
      (ok st)
      (fail st {:fail :eof}))))

(defn eps []
  (fn [st ok _fail]
    (ok st)))
