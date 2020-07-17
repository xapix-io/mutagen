(ns perf-check
  (:require [mutagen.core :as m]
            [mutagen.string :as sm]
            [mutagen.grammars.json :as json]
            [blancas.kern.core :as k]
            [clojure.java.io :as io]
            [criterium.core :as cc]
            [clojure.string :as string]))

;; ========== Mutagen ===============

(def mutagen-grammar
  {:root [::m/cat :instruction [::m/star [::m/cat [::sm/char {:hide true} \,] :instruction]] [::sm/char {:hide true} \newline] [::sm/eof]]
   :instruction [::m/alt :partner :exchange :spin]
   :partner [::m/cat {:wrap #(cons :PARTNER %)} [::sm/char {:hide true} \p] :program [::sm/char {:hide true} \/] :program]
   :exchange [::m/cat {:wrap #(cons :EXCHANGE %)} [::sm/char {:hide true} \x] :position [::sm/char {:hide true} \/] :position]
   :spin [::m/cat {:wrap #(cons :SPIN %)} [::sm/char {:hide true} \s] :position]
   :position [::m/cat {:wrap #(apply str %)} :digit [::m/opt :digit]]
   :program [::sm/char \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p]
   :digit [::sm/char \0 \1 \2 \3 \4 \5 \6 \7 \8 \9]})

(def mutagen-parser
  (let [p (m/-start-production
           (m/grammar mutagen-grammar)
           :root)]
    (fn [st]
      (p (sm/input st)))))

;; ========================
;; =========== Kern ===========

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

;; ========================

(defn but-first-char
  [s]
  (subs s 1 (count s)))

(defn parse-args
  [s]
  (string/split s #"/"))

(defn parse-int
  ([s]
   (parse-int s 10))
  ([s b]
   (try (Integer/parseInt s b)
        (catch Exception e nil))))

(defn handwritten-parser
  [d]
  (map
   (fn [expr]
     (case (first expr)
       \s [:SPIN (parse-int
                  (but-first-char expr))]
       \x (into [:EXCHANGE]
                (mapv parse-int
                      (parse-args
                       (but-first-char expr))))
       \p (into [:PARTNER]
                (mapv
                 first
                 (parse-args
                  (but-first-char expr))))))
   (string/split d #",")))

(comment

  (def st (slurp (io/resource "sample_data")))

  (cc/quick-bench (doall (handwritten-parser st))) ;; ~> 8ms
  (cc/quick-bench (doall (mutagen-parser st)))     ;; ~> 33ms
  (cc/quick-bench (doall (kern-parser st)))        ;; ~> 111ms

  (def json-st (slurp (io/resource "example.json")))

  (cc/quick-bench (first (json/parse json-st)))    ;; ~> 12ms + 13Kb json file

  )
