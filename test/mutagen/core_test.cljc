(ns mutagen.core-test
  (:require [mutagen.core :as sut]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true]))
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(deftest core-test
  (is (= [\f \o \o \b \a \r]
         ((sut/parser [::sut/cat "foo" "bar"])
          "foobar"))))

(deftest epsilon-parser
  (is (= [] ((sut/parser [::sut/eps]) "qwe"))))

(deftest cat-parser
  (is (= [\a \b \c]
         ((sut/parser [::sut/cat
                       [::sut/char \a] [::sut/char {:hide true} \a]
                       [::sut/char \b]
                       [::sut/char \c]])
          "aabc")))
  (is (= [\a]
         ((sut/parser [::sut/cat [::sut/char \a]])
          "aaa"))))

(deftest alt-parser
  (is (= [\c]
         ((sut/parser [::sut/alt
                       [::sut/char \a]
                       [::sut/char \b]
                       [::sut/char \c]])
          "cba")))
  (is (= [\a]
         ((sut/parser [::sut/alt [::sut/char \a]])
          "aaa"))))

(deftest plus-parser
  (is (= ["3A"]
         ((sut/parser [::sut/plus {:wrap #(str (count %) "A")}
                       [::sut/char \a]])
          "aaabbb")))
  (is (= ["1A"]
         ((sut/parser [::sut/plus {:wrap #(str (count %) "A")}
                       [::sut/char \a]])
          "abbb")))
  (is (thrown? ExceptionInfo
               ((sut/parser [::sut/plus {:wrap #(str (count %) "A")}
                             [::sut/char \a]])
                "bbb"))))

(deftest star-parser
  (is (= ["3A"]
         ((sut/parser [::sut/star {:wrap #(str (count %) "A")}
                       [::sut/char \a]])
          "aaabbb")))
  (is (= ["1A"]
         ((sut/parser [::sut/star {:wrap #(str (count %) "A")}
                       [::sut/char \a]])
          "abbb")))
  (is (= ["0A"]
         ((sut/parser [::sut/star {:wrap #(str (count %) "A")}
                       [::sut/char \a]])
          "bbb"))))

(deftest rep-parser
  (is (= [\a \a \a]
         ((sut/parser [::sut/rep {:times 3} \a])
          "aaa")))
  (is (thrown? ExceptionInfo
               ((sut/parser [::sut/rep {:times 3} \a])
                "aa"))))

(deftest lookahead-parser
  (is (= []
         ((sut/parser [::sut/la [::sut/char \a]])
          "aaa")))
  (is (thrown? ExceptionInfo
               ((sut/parser [::sut/la [::sut/char \a]])
                "baa"))))

(deftest negative-lookahead-parser
  (is (= []
         ((sut/parser [::sut/nla [::sut/char \a]])
          "baa")))
  (is (thrown? ExceptionInfo
               ((sut/parser [::sut/nla [::sut/char \a]])
                "aaa"))))

(deftest keep-parser
  (is (= [\a \a \a]
         ((sut/parser [::sut/rep {:times 3} [::sut/keep \a]])
          "a"))))
