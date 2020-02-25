(ns samples.json
  (:refer-clojure :exclude [array boolean])
  (:require #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
            [mutagen.core :as m #?@(:cljs [:require-macros true])]))

(defn wrap-const [token content]
  [(assoc token :content content)])

(defn const-token [xs]
  {:range (mapv (juxt :line :col) [(first xs)
                                   (last xs)])
   :node :const})

(defn wrap-null [xs]
  (wrap-const
   (const-token xs)
   nil))

(defn wrap-boolean [xs]
  (wrap-const
   (const-token xs)
   (case (-> xs first :ch)
     \t true
     \f false)))

(defn wrap-number [xs]
  (wrap-const
   (const-token xs)
   (edn/read-string (apply str (map :ch xs)))))

(defn wrap-string [xs]
  (wrap-const
   (const-token xs)
   (apply str (map :ch (rest (butlast xs))))))

(defn wrap-escape-char [xs]
  [(update (second xs) :ch {\" \"
                            \\ \\
                            \b \backspace
                            \f \formfeed
                            \n \newline
                            \r \return
                            \t \tab})])

(defn wrap-unicode-char [xs]
  [(assoc (last xs) :ch
          (char (#?(:clj Integer/parseInt :cljs js/parseInt)
                 (apply str (map :ch (drop 2 xs))) 16)))])

(defn wrap-array [xs]
  [{:range (mapv (juxt :line :col) [(first xs) (last xs)])
    :node :array
    :content (vec (rest (butlast xs)))}])

(defn wrap-member [xs]
  [{:range [(-> xs first :range first)
            (-> xs last :range last)]
    :content xs
    :node :member}])

(defn wrap-object [xs]
  [{:range (mapv (juxt :line :col) [(first xs) (last xs)])
    :node :object
    :content (vec (rest (butlast xs)))}])

(declare json)

(def ws (m/skip
         (m/star (apply m/some-char [\space \backspace \formfeed \newline \return \tab]))))

(def null (m/wrap
           (m/cat
            (m/word "null")
            ws)
           {:wrap-res wrap-null}))

(def boolean (m/wrap
              (m/cat
               (m/alt
                (m/word "true")
                (m/word "false"))
               ws)
              {:wrap-res wrap-boolean}))

(def non-zero-digit (apply m/some-char (m/char-range \1 \9)))

(def digit (m/alt
            (m/char1 \0)
            non-zero-digit))

(def integer (m/alt
              (m/cat
               non-zero-digit
               (m/star digit))
              (m/char1 \0)))

(def number (m/wrap
             (m/cat
              integer
              (m/opt
               (m/cat
                (m/char1 \.)
                (m/plus digit)))
              (m/opt
               (m/cat
                (apply m/some-char [\e \E])
                (m/opt
                 (apply m/some-char [\- \+]))
                integer))
              ws)
             {:wrap-res wrap-number}))

(def string (m/wrap
             (m/cat
              (m/char1 \")
              (m/star
               (m/alt
                (m/wrap
                 (m/cat
                  (m/char1 \\ )
                  (apply m/some-char [\" \\ \b \f \n \r \t]))
                 {:wrap-res wrap-escape-char})
                (m/wrap
                 (m/cat
                  (m/char1 \\ )
                  (m/char1 \u)
                  (m/rep 4 (apply m/some-char (concat (m/char-range \0 \9)
                                                      (m/char-range \a \f)
                                                      (m/char-range \A \F)))))
                 {:wrap-res wrap-unicode-char})
                (m/cat
                 (m/neg (m/char1 \"))
                 m/any-char)))
              (m/char1 \")
              ws)
             {:wrap-res wrap-string}))

(def array (m/wrap
            (m/cat
             (m/char1 \[)
             ws
             (m/alt
              (m/cat
               (m/resolve json)
               (m/star
                (m/cat
                 (m/skip
                  (m/char1 \,))
                 ws
                 (m/resolve json))))
              m/eps)
             (m/char1 \])
             ws)
            {:wrap-res wrap-array}))

(def member (m/wrap
             (m/cat
              string
              (m/skip
               (m/char1 \:))
              ws
              (m/resolve json))
             {:wrap-res wrap-member}))

(def object (m/wrap
             (m/cat
              (m/char1 \{)
              ws
              (m/alt
               (m/cat
                member
                (m/star
                 (m/cat
                  (m/skip
                   (m/char1 \,))
                  ws
                  member)))
               m/eps)
              (m/char1 \})
              ws)
             {:wrap-res wrap-object}))

(def json (m/alt
           null
           boolean
           string
           number
           object
           array))

(def parser (m/parser (m/cat json m/eof)))

(defn parse [json-string]
  (parser json-string :out (fn [_ failure] failure)))

(comment

  (parse "[1,2,3]")

  )
