(ns perf-check
  (:require [mutagen.lexers.string :as string]
            [mutagen.combinators :as comb]
            [blancas.kern.core :as k]
            [clojure.java.io :as io]
            [criterium.core :as cc]))

(def digit
  (string/char* (string/char-range \0 \9)))

(def program
  (string/char* (string/char-range \a \p)))

(def position
  (comb/cat digit (comb/opt digit)))

(def partner
  (comb/wrap
   (comb/cat
    (comb/discard (string/char1 \p))
    program
    (comb/discard (string/char1 \/))
    program)
   {:ok-wrapper (fn [xs] [(cons :PARTNER xs)])}))

(def exchange
  (comb/wrap
   (comb/cat
    (comb/discard (string/char1 \x))
    position
    (comb/discard (string/char1 \/))
    position)
   {:ok-wrapper (fn [xs] [(cons :EXCHANGE xs)])}))

(def spin
  (comb/wrap
   (comb/cat
    (comb/discard (string/char1 \s))
    position)
   {:ok-wrapper (fn [xs] [(cons :SPIN xs)])}))

(def instruction
  (comb/alt
   partner
   exchange
   spin))

(def root
  (comb/cat
   instruction
   (comb/star
    (comb/cat
     (comb/discard (string/char1 \,))
     instruction))))

(def parser (string/parser root))

(defn mutagen-parser [st]
  (:result (parser st)))

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

  (cc/bench (doall (mutagen-parser st)))

  (cc/bench (doall (kern-parser st)))

  )
