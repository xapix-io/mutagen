(ns mutagen.simple-test
  (:require [mutagen.core :as m]
            [mutagen.string :as sm]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])))

(def simple-parser
  (m/-start-production
   (m/grammar {:A [::m/plus {:wrap #(str (count %) "A")}
                   [::sm/char \a]]
               :B [::m/plus {:wrap #(str (count %) "B")}
                   [::sm/char \b]]
               :S [::m/star [::m/cat :A :B]]})
   :S))

(deftest basic
  (is (= []
         (simple-parser (sm/input ""))))
  (is (= ["2A" "1B"]
         (simple-parser (sm/input "aab"))))
  (is (= ["3A" "2B" "1A" "1B"]
         (simple-parser (sm/input "aaabbab")))))
