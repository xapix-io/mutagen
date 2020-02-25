(ns mutagen.simple-test
  (:require [mutagen.core :as m]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])))

(def A (m/wrap
        (m/plus (m/char \a))
        {:wrap-res (fn [xs] [(str (count xs) "A")])}))

(def B (m/wrap
        (m/plus (m/char \b))
        {:wrap-res (fn [xs] [(str (count xs) "B")])}))

(deftest basic
  (let [P (m/parser (m/star (m/cat A B)))]
   (is (= []
          (P "" :out (fn [_ failure] failure))))
   (is (= ["2A" "1B"]
          (P "aab" :out (fn [_ failure] failure))))
   (is (= ["3A" "2B" "1A" "1B"]
          (P "aaabbab" :out (fn [_ failure] failure))))))
