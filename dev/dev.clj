(ns dev
  (:require [samples.json]
            [mutagen.parsers.string :as string-parser]))

(def G
  {"X" [:alt
        [:char \q]
        [:char \w]
        [:char \e]]})

(comment

  (let [p (string-parser/parser G ["X"])]
    (p "r"))

  )
