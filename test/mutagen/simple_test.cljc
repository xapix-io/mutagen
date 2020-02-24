(ns mutagen.simple-test
  (:require [mutagen.lexers.indexed-string :as string-parser]
            [mutagen.combinators :as comb]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])))

(def A (comb/wrap
        (comb/plus (string-parser/char1 \a))
        {:ok-wrapper (fn [xs] [(str (count xs) "A")])}))

(def B (comb/wrap
        (comb/plus (string-parser/char1 \b))
        {:ok-wrapper (fn [xs] [(str (count xs) "B")])}))

(deftest basic
  (let [P (string-parser/parser
           (comb/star (comb/cat A B)))]
   (is (= []
          (:result (P ""))))
   (is (= ["2A" "1B"]
          (:result (P "aab"))))
   (is (= ["3A" "2B" "1A" "1B"]
          (:result (P "aaabbab"))))))
