(ns mutagen.simple-test
  (:require [mutagen.parsers.string :as string-parser]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])))

(deftest basic
  (let [P (string-parser/parser
           {"A" [:plus {:ok-wrapper (fn [xs] [(str (count xs) "A")])}
                 [:char \a]]
            "B" [:plus {:ok-wrapper (fn [xs] [(str (count xs) "B")])}
                 [:char \b]]}
           [:star [:cat ["A"] ["B"]]])]
   (is (= []
          (:result (P ""))))
   (is (= ["2A" "1B"]
          (:result (P "aab"))))
   (is (= ["3A" "2B" "1A" "1B"]
          (:result (P "aaabbab"))))))
