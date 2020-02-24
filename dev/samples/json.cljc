(ns samples.json
  (:refer-clojure :exclude [array boolean])
  (:require [mutagen.lexers.indexed-string :as string]
            [mutagen.lexers.string :refer [char-range]]
            #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
            [mutagen.combinators :as comb]))

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

(def ws (comb/discard
         (comb/star (string/char* [\space \backspace \formfeed \newline \return \tab]))))

(def null (comb/wrap
           (comb/cat
            (string/word "null")
            ws)
           {:ok-wrapper wrap-null}))

(def boolean (comb/wrap
              (comb/cat
               (comb/alt
                (string/word "true")
                (string/word "false"))
               ws)
              {:ok-wrapper wrap-boolean}))

(def non-zero-digit (string/char* (char-range \1 \9)))

(def digit (comb/alt
            (string/char1 \0)
            non-zero-digit))

(def integer (comb/alt
              (comb/cat
               non-zero-digit
               (comb/star digit))
              (string/char1 \0)))

(def number (comb/wrap
             (comb/cat
              integer
              (comb/opt
               (comb/cat
                (string/char1 \.)
                (comb/plus digit)))
              (comb/opt
               (comb/cat
                (string/char* [\e \E])
                (comb/opt
                 (string/char* [\- \+]))
                integer))
              ws)
             {:ok-wrapper wrap-number}))

(def string (comb/wrap
             (comb/cat
              (string/char1 \")
              (comb/star
               (comb/alt
                (comb/wrap
                 (comb/cat
                  (string/char1 \\ )
                  (string/char* [\" \\ \b \f \n \r \t]))
                 {:ok-wrapper wrap-escape-char})
                (comb/wrap
                 (comb/cat
                  (string/char1 \\ )
                  (string/char1 \u)
                  (comb/rep 4 (string/char* (concat (char-range \0 \9)
                                                    (char-range \a \f)
                                                    (char-range \A \F)))))
                 {:ok-wrapper wrap-unicode-char})
                (comb/cat
                 (comb/neg (string/char1 \"))
                 (string/any-char))))
              (string/char1 \")
              ws)
             {:ok-wrapper wrap-string}))

(defn array [itemp]
  (comb/wrap
   (comb/cat
    (string/char1 \[)
    ws
    (comb/alt
     (comb/cat
      itemp
      (comb/star
       (comb/cat
        (comb/discard
         (string/char1 \,))
        ws
        itemp)))
     (comb/eps))
    (string/char1 \])
    ws)
   {:ok-wrapper wrap-array}))

(defn member [keyp valuep]
  (comb/wrap
   (comb/cat
    keyp
    (comb/discard
     (string/char1 \:))
    ws
    valuep)
   {:ok-wrapper wrap-member}))

(defn object [keyp valuep]
  (let [memberp (member keyp valuep)]
    (comb/wrap
     (comb/cat
      (string/char1 \{)
      ws
      (comb/alt
       (comb/cat
        memberp
        (comb/star
         (comb/cat
          (comb/discard
           (string/char1 \,))
          ws
          memberp)))
       (comb/eps))
      (string/char1 \})
      ws)
     {:ok-wrapper wrap-object})))

(def json
  (comb/alt
   null
   boolean
   string
   number
   (object string json)
   (array json)))

(def parser (string/parser (comb/cat json (comb/eof))))

(defn parse [json-string]
  (parser json-string))

(comment

  (parse "[1,2,3]")

  ((string/parser boolean) "false")

  )
