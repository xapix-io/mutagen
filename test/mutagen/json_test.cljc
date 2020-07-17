(ns mutagen.json-test
  (:require [mutagen.core :as m]
            [mutagen.grammars.json :as json]
            #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is] :include-macros true])))

(deftest scalars
  (testing "empty string"
    (is (= []
           (json/parse ""))))

  (testing "null"
    (is (= [nil]
           (json/parse "null"))))

  (testing "booleans"
    (is (= [true]
           (json/parse "true")))
    (is (= [false]
           (json/parse "false"))))

  (testing "numbers"

    (testing "integer"
      (is (= [0] (json/parse "0")))
      (is (= [1] (json/parse "1")))
      (is (= [42] (json/parse "42"))))

    (testing "floats"
      (is (= [0.0] (json/parse "0.0")))
      (is (= [1.0123]
             (json/parse "1.0123")))
      (is (= [42.321]
             (json/parse "42.321"))))

    (testing "exponential format"
      (is (= [0.0]
             (json/parse "0.0e0")))
      (is (= [1.0E-12]
             (json/parse "1.0E-12")))
      (is (= [4.2E11]
             (json/parse "0.42e+12")))))

  (testing "strings"

    (testing "empty"
      (is (= [""]
             (json/parse "\"\""))))

    (testing "simple"
      (is (= ["foo"]
             (json/parse "\"foo\""))))

    (testing "with unicode characters"
      (is (= ["﷽"]
             (json/parse "\"\\uFDFD\""))))

    (testing "with escape characters"
      (is (= ["hello world\"\b\f\n\r\totherchars"]
             (json/parse "\"hello world\\\"\\b\\f\\n\\r\\totherchars\""))))))

(deftest collections
  (testing "empty array"
    (is (= [[]]
           (json/parse "[]"))))

  (testing "array"
    (is (= [[nil true false 0 42 42.01 0.04201 "foo" [] [1 2 3] {} {"foo" 1}]]
           (json/parse "[
  null, true,
false,
0, 42, 42.01, 42.01e-3
, \"foo\",
[], [1,2,3],
{}, {\"foo\": 1}
]"))))

  (testing "empty object"
    (is (= [{}]
           (json/parse "{}"))))

  (testing "object"
    (is (= [{"boolean" true,
             " nested objects ﷽" {"nested" true},
             "string" "string",
             "boolean2" false,
             "null" nil,
             "array of numbers" [0 1 42 42.32]}]
           (json/parse "{\"string\": \"string\", \"null\":null
,\"boolean\":
true,
\"boolean2\"    :     false,
\"array of numbers\":[0, 1, 42, 42.32],
\" nested objects \\ufdfd\": {\"nested\": true}
}")))))
