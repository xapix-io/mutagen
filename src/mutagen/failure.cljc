(ns mutagen.failure
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(defn unexpected-success [st]
  (ex-info "Expect parser to fail but got result instead." {:state st
                                                            :type :parse-fail}))

(defn unexpected-eof [st]
  (ex-info "Unexpected end of input." {:state st
                                       :type :parse-fail}))

(defn unexpected-token [st token]
  (ex-info (str "Unexpected token `" token "`.") {:state st
                                                  :type :parse-fail}))

(defn fail? [obj]
  (and (= (type obj) ExceptionInfo) (= :parse-fail (:type (ex-data obj)))))
