(ns perf-check
  (:require [mutagen.combinators :as m]
            [mutagen.grammar :as g]
            [blancas.kern.core :as k]
            [clojure.java.io :as io]
            [criterium.core :as cc]))

(g/defgrammar
  {:digit [:some-char [:range \0 \9]]
   :program [:some-char [:range \a \p]]
   :position [:cat {:wrap-res (fn [xs] [(apply str (map :ch xs))])}
              [:digit]
              [:opt [:digit]]]
   :partner [:cat {:wrap-res (fn [xs] [(cons :PARTNER (map (comp str :ch) xs))])}
             [:skip [:char \p]]
             [:program]
             [:skip [:char \/]]
             [:program]]
   :exchange [:cat {:wrap-res (fn [xs] [(cons :EXCHANGE xs)])}
              [:skip [:char \x]]
              [:position]
              [:skip [:char \/]]
              [:position]]
   :spin [:cat {:wrap-res (fn [xs] [(cons :SPIN xs)])}
          [:skip [:char \s]]
          [:position]]
   :instruction [:alt
                 [:partner]
                 [:exchange]
                 [:spin]]
   :S [:cat
       [:instruction]
       [:star
        [:cat
         [:skip [:char \,]]
         [:instruction]]]
       [:skip [:char \newline]]
       [:eof]]})

(def mutagen-parser (m/parser S))

(def parse-partner
  (k/bind [_  (k/sym* \p)
           p1 k/letter
           _ (k/sym* \/)
           p2 k/letter]
          (k/return [:PARTNER p1 p2])))

(def parse-exchange
  (k/bind [_ (k/sym* \x)
           pos1 k/dec-num
           _ (k/sym* \/)
           pos2 k/dec-num]
          (k/return [:EXCHANGE pos1 pos2])))

(def parse-spin
  (k/bind [_ (k/sym* \s) n k/dec-num]
          (k/return [:SPIN n])))

(def parse-instruction
  (k/<|> parse-spin parse-exchange parse-partner))

(def parse-input
  (k/sep-by1 (k/sym* \,) parse-instruction))

(defn kern-parser
  [d]
  (:value (k/parse-data parse-input d)))

(comment

  ;; GOAL: 7ms - 110ms

  (def st (slurp (io/resource "sample_data")))

  ((m/parser S) st :out prn)

  (cc/quick-bench (doall (mutagen-parser st :out (fn [_ failure] failure))))

  (cc/quick-bench (doall (kern-parser st)))

  )
