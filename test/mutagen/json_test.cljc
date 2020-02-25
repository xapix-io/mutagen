(ns mutagen.json-test
  (:require [samples.json :as json]
            #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is] :include-macros true])))

(deftest scalars
  (testing "empty string"
    (is (= [] (json/parse ""))))

  (testing "null"
    (is (= [{:range [[1 1] [1 4]], :node :const, :content nil}]
           (json/parse "null"))))

  (testing "booleans"
    (is (= [{:range [[1 1] [1 4]], :node :const, :content true}]
           (json/parse "true")))
    (is (= [{:range [[1 1] [1 5]], :node :const, :content false}]
           (json/parse "false"))))

  (testing "numbers"

    (testing "integer"
      (is (= [{:range [[1 1] [1 1]], :node :const, :content 0}]
             (json/parse "0")))
      (is (= [{:range [[1 1] [1 1]], :node :const, :content 1}]
             (json/parse "1")))
      (is (= [{:range [[1 1] [1 2]], :node :const, :content 42}]
             (json/parse "42"))))

    (testing "floats"
      (is (= [{:range [[1 1] [1 3]], :node :const, :content 0.0}]
             (json/parse "0.0")))
      (is (= [{:range [[1 1] [1 6]], :node :const, :content 1.0123}]
             (json/parse "1.0123")))
      (is (= [{:range [[1 1] [1 6]], :node :const, :content 42.321}]
             (json/parse "42.321"))))

    (testing "exponential format"
      (is (= [{:range [[1 1] [1 5]], :node :const, :content 0.0}]
             (json/parse "0.0e0")))
      (is (= [{:range [[1 1] [1 7]], :node :const, :content 1.0E-12}]
             (json/parse "1.0E-12")))
      (is (= [{:range [[1 1] [1 8]], :node :const, :content 4.2E11}]
             (json/parse "0.42e+12")))))

  (testing "strings"

    (testing "empty"
      (is (= [{:range [[1 1] [1 2]], :node :const, :content ""}]
             (json/parse "\"\""))))

    (testing "simple"
      (is (= [{:range [[1 1] [1 5]], :node :const, :content "foo"}]
             (json/parse "\"foo\""))))

    (testing "with unicode characters"
      (is (= [{:range [[1 1] [1 8]], :node :const, :content "﷽"}]
             (json/parse "\"\\uFDFD\""))))

    (testing "with escape characters"
      (is (= [{:range [[1 1] [1 35]],
               :node :const,
               :content "hello world\"\b\f\n\r\totherchars"}]
             (json/parse "\"hello world\\\"\\b\\f\\n\\r\\totherchars\""))))))

(deftest collections
  (testing "empty array"
    (is (= [{:range [[1 1] [1 2]], :node :array, :content []}]
           (json/parse "[]"))))

  (testing "array"
    (is (= [{:range [[1 1] [8 0]],
            :node :array,
            :content
            [{:range [[2 2] [2 5]], :node :const, :content nil}
             {:range [[2 8] [2 11]], :node :const, :content true}
             {:range [[3 0] [3 4]], :node :const, :content false}
             {:range [[4 0] [4 0]], :node :const, :content 0}
             {:range [[4 3] [4 4]], :node :const, :content 42}
             {:range [[4 7] [4 11]], :node :const, :content 42.01}
             {:range [[4 14] [4 21]], :node :const, :content 0.04201}
             {:range [[5 2] [5 6]], :node :const, :content "foo"}
             {:range [[6 0] [6 1]], :node :array, :content []}
             {:range [[6 4] [6 10]],
              :node :array,
              :content
              [{:range [[6 5] [6 5]], :node :const, :content 1}
               {:range [[6 7] [6 7]], :node :const, :content 2}
               {:range [[6 9] [6 9]], :node :const, :content 3}]}
             {:range [[7 0] [7 1]], :node :object, :content ()}
             {:range [[7 4] [7 13]],
              :node :object,
              :content
              [{:range [[7 5] [7 12]],
                :content
                [{:range [[7 5] [7 9]], :node :const, :content "foo"}
                 {:range [[7 12] [7 12]], :node :const, :content 1}],
                :node :member}]}]}]
           (json/parse "[
  null, true,
false,
0, 42, 42.01, 42.01e-3
, \"foo\",
[], [1,2,3],
{}, {\"foo\": 1}
]"))))

  (testing "empty object"
    (is (= [{:range [[1 1] [1 2]], :node :object, :content []}]
           (json/parse "{}"))))

  (testing "object"
    (is (= [{:range [[1 1] [7 0]],
            :node :object,
            :content
            [{:range [[1 2] [1 19]],
              :content
              [{:range [[1 2] [1 9]], :node :const, :content "string"}
               {:range [[1 12] [1 19]], :node :const, :content "string"}],
              :node :member}
             {:range [[1 22] [1 32]],
              :content
              [{:range [[1 22] [1 27]], :node :const, :content "null"}
               {:range [[1 29] [1 32]], :node :const, :content nil}],
              :node :member}
             {:range [[2 1] [3 3]],
              :content
              [{:range [[2 1] [2 9]], :node :const, :content "boolean"}
               {:range [[3 0] [3 3]], :node :const, :content true}],
              :node :member}
             {:range [[4 0] [4 24]],
              :content
              [{:range [[4 0] [4 9]], :node :const, :content "boolean2"}
               {:range [[4 20] [4 24]], :node :const, :content false}],
              :node :member}
             {:range [[5 0] [5 35]],
              :content
              [{:range [[5 0] [5 17]], :node :const, :content "array of numbers"}
               {:range [[5 19] [5 35]],
                :node :array,
                :content
                [{:range [[5 20] [5 20]], :node :const, :content 0}
                 {:range [[5 23] [5 23]], :node :const, :content 1}
                 {:range [[5 26] [5 27]], :node :const, :content 42}
                 {:range [[5 30] [5 34]], :node :const, :content 42.32}]}],
              :node :member}
             {:range [[6 0] [6 41]],
              :content
              [{:range [[6 0] [6 23]],
                :node :const,
                :content " nested objects ﷽"}
               {:range [[6 26] [6 41]],
                :node :object,
                :content
                [{:range [[6 27] [6 40]],
                  :content
                  [{:range [[6 27] [6 34]], :node :const, :content "nested"}
                   {:range [[6 37] [6 40]], :node :const, :content true}],
                  :node :member}]}],
              :node :member}]}]
           (json/parse "{\"string\": \"string\", \"null\":null
,\"boolean\":
true,
\"boolean2\"    :     false,
\"array of numbers\":[0, 1, 42, 42.32],
\" nested objects \\ufdfd\": {\"nested\": true}
}")))))
