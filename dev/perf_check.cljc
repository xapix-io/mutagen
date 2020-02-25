(ns perf-check
  (:require [mutagen.core :as m]
            [blancas.kern.core :as k]
            [clojure.java.io :as io]
            [criterium.core :as cc]))

(def digit
  (apply m/some-char (m/char-range \0 \9)))

(def program
  (apply m/some-char (m/char-range \a \p)))

(def position
  (m/wrap
   (m/cat digit (m/opt digit))
   {:wrap-res (fn [xs] [(apply str (map :ch xs))])}))

(def partner
  (m/wrap
   (m/cat
    (m/skip (m/char \p))
    program
    (m/skip (m/char \/))
    program)
   {:wrap-res (fn [xs] [(cons :PARTNER (map (comp str :ch) xs))])}))

(def exchange
  (m/wrap
   (m/cat
    (m/skip (m/char \x))
    position
    (m/skip (m/char \/))
    position)
   {:wrap-res (fn [xs] [(cons :EXCHANGE xs)])}))

(def spin
  (m/wrap
   (m/cat
    (m/skip (m/char \s))
    position)
   {:wrap-res (fn [xs] [(cons :SPIN xs)])}))

(def instruction
  (m/alt
   partner
   exchange
   spin))

(def root
  (m/cat
   instruction
   (m/star
    (m/cat
     (m/skip (m/char \,))
     instruction))
   (m/skip (m/char \newline))
   m/eof))

(def mutagen-parser (m/parser root))

;; Kern parser

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

  (cc/quick-bench (doall (mutagen-parser st :out (fn [_ failure] failure))))

  (cc/quick-bench (doall (kern-parser st)))

  )
