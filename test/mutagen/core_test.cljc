(ns mutagen.core-test
  (:require [mutagen.combinators :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest char-range-test
  (t/is (= '(\a \b \c \d \e \f)
           (sut/char-range \a \f))))
