(ns mutagen.core
  (:refer-clojure :exclude [cat keep]))

(defprotocol Parser
  (-form [this])
  (-parser [this]))

(defprotocol Grammar
  (-start-production [this k]))

(defn cat
  ([P]
   (fn [in out ok fail]
     ((-parser P) in out ok fail)))
  ([P & PS]
   (reduce
    (fn [f P]
      (fn [in out ok fail]
        (f in out
         (fn [in out]
           #((-parser P) in out ok fail))
         fail)))
    (list* (cat P) PS))))

(defn eps []
  (fn [in out ok _fail]
    #(ok in out)))

(defn alt
  ([P]
   (fn [in out ok fail]
     ((-parser P) in out ok fail)))
  ([P & PS]
   (reduce
    (fn [f P]
      (fn [in out ok fail]
        (f in out
         ok (fn [_ _]
              #((-parser P) in out
                ok fail)))))
    (list* (alt P) PS))))

(defn star [P]
  (fn star* [in out ok fail]
    (if (empty? in)
      (ok in out)
      ((-parser P) in out
       (fn [in out]
         #(star* in out ok fail))
       (fn [_ _]
         #(ok in out))))))

(defn plus [P]
  (let [P* (star P)]
    (fn [in out ok fail]
      ((-parser P) in out
       (fn [in out]
         #(P* in out ok fail))
       fail))))

(defn rep [N P]
  (apply cat (repeat N P)))

(defn opt [P]
  (fn [in out ok _fail]
    ((-parser P) in out
     ok (fn [_ _]
          #(ok in out)))))

(defn neg [P]
  (fn [in out ok fail]
    ((-parser P) in out
     (fn [_ _]
       #(fail in out))
     (fn [_ _]
       #(ok in out)))))

(defn skip [P]
  (fn [in out ok fail]
    ((-parser P) in out
     (fn [in _]
       #(ok in out))
     fail)))

(defn lookahead [P]
  (fn [in out ok fail]
    ((-parser P) in out
     (fn [_ _]
       #(ok in out))
     fail)))

(defn keep [P]
  (fn [in out ok fail]
    ((-parser P) in out
     (fn [_ out]
       #(ok in out))
     fail)))

(defn with-wrapper [parser wrap-fn]
  (reify Parser
    (-form [_] (-form parser))
    (-parser [_]
      (fn [in out ok fail]
        ((-parser parser)
         in (empty out)
         (fn [in out']
           #(ok in (conj out (wrap-fn out'))))
         fail)))))

(defn with-hiding [parser]
  (reify Parser
    (-form [_] (-form parser))
    (-parser [_]
      (fn [in out ok fail]
        ((-parser parser)
         in out
         (fn [in _]
           #(ok in out))
         fail)))))

(defn save-to-registry! [parser ref registry]
  (if (contains? @registry ref)
    (throw (ex-info "Reference already defined!" {:ref ref :parser parser}))
    (do (vswap! registry assoc ref parser)
        parser)))

(defmulti parser* (fn [_ form]
                    (cond
                      (vector? form) :vector
                      (keyword? form) :keyword)))

(defmulti vec->parser (fn [t & _] t))

(defmethod vec->parser :default [t {:keys [registry]} props & args]
  (let [form (into [t props] args)]
    (reify Parser
      (-form [_] form)
      (-parser [_]
        (or (some-> (get @registry t) (-parser)) (throw (ex-info "Unknown parser" {:form form})))))))

(defmethod vec->parser ::eps [& _]
  (let [form [::eps]
        pf (eps)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod vec->parser ::cat [_ opts props children]
  (let [children (map (partial parser* opts) children)
        form (into [::cat props] (map -form children))
        pf (apply cat children)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod vec->parser ::alt [_ opts props children]
  (let [children (map (partial parser* opts) children)
        form (into [::alt props] (map -form children))
        pf (apply alt children)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod vec->parser ::star [_ opts props [child]]
  (let [child (parser* opts child)
        form [::star props (-form child)]
        pf (star child)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod vec->parser ::plus [_ opts props [child]]
  (let [child (parser* opts child)
        form [::plus props (-form child)]
        pf (plus child)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod vec->parser ::rep [_ opts {:keys [times] :as props} [child]]
  (let [child (parser* opts child)
        form [::rep props (-form child)]
        pf (rep times child)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod vec->parser ::opt [_ opts props [child]]
  (let [child (parser* opts child)
        form [::opt props (-form child)]
        pf (opt child)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod vec->parser ::neg [_ opts props [child]]
  (let [child (parser* opts child)
        form [::neg props (-form child)]
        pf (opt child)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod vec->parser ::keep [_ opts props [child]]
  (let [child (parser* opts child)
        form [::keep props (-form child)]
        pf (keep child)]
    (reify Parser
      (-form [_] form)
      (-parser [_] pf))))

(defmethod parser* :vector [{:keys [registry] :as opts} [t & children]]
  (let [{:keys [id wrap hide] :as props} (if (map? (first children)) (first children) {})
        children                         (if (map? (first children)) (rest children) children)
        parser                           (vec->parser t opts props children)]
    (cond-> parser
      hide                  (with-hiding)
      (and wrap (not hide)) (with-wrapper wrap)
      id                    (save-to-registry! id registry))))

(defmethod parser* :keyword [opts reference]
  (parser* opts [reference {}]))

(defn parser
  ([form] (parser {:registry (volatile! {})} form))
  ([opts form]
   (parser* opts form)))

(defn- default-ok [_in out]
  out)

(defn- default-fail [in out]
  (throw (ex-info "Can not parse input" {:in in :out out})))

(defn grammar [gm]
  (let [{:keys [registry]}
        (reduce-kv
         (fn [{:keys [registry] :as opts} k form]
           (save-to-registry! (parser opts form) k registry)
           opts)
         {:registry (volatile! {})}
         gm)]
    (reify Grammar
      (-start-production [this k]
        (let [p (-parser (get @registry k))]
          (fn f
            ([st] (f st default-ok default-fail))
            ([st ok] (f st ok default-fail))
            ([st ok fail]
             (trampoline p st [] ok fail))))))))
