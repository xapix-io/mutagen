(ns mutagen.state)

(defmulti eof? :type)

(defmethod eof? :default [st]
  (throw (ex-info (str "Undefined state type `" (:type st) "`.") {:state st})))
