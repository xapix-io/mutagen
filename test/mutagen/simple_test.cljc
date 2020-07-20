(ns mutagen.simple-test
  (:require [mutagen.core :as m]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])))

(def simple-parser
  (m/-parser
   (m/grammar {:A [::m/plus {:wrap #(str (count %) "A")}
                   [::m/char \a]]
               :B [::m/plus {:wrap #(str (count %) "B")}
                   [::m/char \b]]
               :S [::m/star [::m/cat :A :B]]})
   :S))

(deftest basic
  (is (= []
         (simple-parser "")))
  (is (= ["2A" "1B"]
         (simple-parser "aab")))
  (is (= ["3A" "2B" "1A" "1B"]
         (simple-parser "aaabbab"))))
