(ns samples.json
  (:refer-clojure :exclude [array boolean])
  (:require #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
            [mutagen.combinators :as mc]))

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

(defn wrap-pair [xs]
  [{:range [(-> xs first :range first)
            (-> xs last :range last)]
    :content xs
    :node :pair}])

(defn wrap-object [xs]
  [{:range (mapv (juxt :line :col) [(first xs) (last xs)])
    :node :object
    :content (vec (rest (butlast xs)))}])

(declare json)

(def ws
  (mc/skip
   (mc/star
    (mc/some-char
     \space
     \backspace
     \formfeed
     \newline
     \return
     \tab))))

(def null
  (mc/wrap
   (mc/cat
    (mc/word "null")
    ws)
   {:wrap-res wrap-null}))

(def boolean
  (mc/wrap
   (mc/cat
    (mc/alt
     (mc/word "true")
     (mc/word "false"))
    ws)
   {:wrap-res wrap-boolean}))

(def non-zero-digit
  (mc/some-char \1 \2 \3 \4 \5 \6 \7 \8 \9))

(def digit
  (mc/alt
   (mc/char1 \0)
   non-zero-digit))

(def integer
  (mc/cat
   (mc/opt (mc/some-char \- \+))
   (mc/alt
    (mc/cat
     non-zero-digit
     (mc/star digit))
    (mc/char1 \0))))

(def number
  (mc/wrap
   (mc/cat
    integer
    (mc/opt
     (mc/cat
      (mc/char1 \.)
      (mc/plus digit)))
    (mc/opt
     (mc/cat
      (mc/some-char \e \E)
      integer))
    ws)
   {:wrap-res wrap-number}))

(def string
  (mc/wrap
   (mc/cat
    (mc/char1 \")
    (mc/star
     (mc/alt
      (mc/wrap
       (mc/cat
        (mc/char1 \\)
        (mc/some-char \" \\ \b \f \n \r \t))
       {:wrap-res wrap-escape-char})
      (mc/wrap
       (mc/cat
        (mc/char1 \\)
        (mc/char1 \u)
        (mc/rep
         4
         (mc/some-char
          \0
          \1
          \2
          \3
          \4
          \5
          \6
          \7
          \8
          \9
          \a
          \b
          \c
          \d
          \e
          \f
          \A
          \B
          \C
          \D
          \E
          \F)))
       {:wrap-res wrap-unicode-char})
      (mc/cat
       (mc/neg (mc/char1 \"))
       (mc/any-char))))
    (mc/char1 \")
    ws)
   {:wrap-res wrap-string}))

(def array
  (mc/wrap
   (mc/cat
    (mc/char1 \[)
    ws
    (mc/alt
     (mc/cat
      (mc/resolve-combinator json)
      (mc/star
       (mc/cat
        (mc/skip
         (mc/char1 \,))
        ws
        (mc/resolve-combinator json)))
      (mc/char1 \]))
     (mc/char1 \]))
    ws)
   {:wrap-res wrap-array}))

(def pair
  (mc/cat
   string
   (mc/skip (mc/char1 \:))
   ws
   (mc/resolve-combinator json)))

(def object
  (mc/wrap
   (mc/cat
    (mc/char1 \{)
    ws
    (mc/alt
     (mc/cat
      pair
      (mc/star
       (mc/cat
        (mc/skip
         (mc/char1 \,))
        ws
        pair))
      (mc/char1 \}))
     (mc/char1 \}))
    ws)
   {:wrap-res wrap-object}))

(def json
  (mc/alt
   null
   boolean
   string
   number
   array
   object))

(def json-document
  (mc/cat
   ws
   (mc/alt json (mc/eps))
   (mc/eof)))

(def json-documents
  (mc/cat
   ws
   (mc/plus json)
   (mc/eof)))

(defn JSON [start-production]
  (mc/parser
   (get
    {:non-zero-digit non-zero-digit,
     :ws ws,
     :number number,
     :pair pair,
     :json-documents json-documents,
     :string string,
     :array array,
     :json-document json-document,
     :integer integer,
     :null null,
     :json json,
     :boolean boolean,
     :object object,
     :digit digit}
    start-production)))

(def parse (JSON :json-document))
