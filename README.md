# mutagen

> Yet another LL(k) parser generator.

## TLDR;

```clojure
;; Example of parsing arbitrary string using `mutagen.parsers.string`
;; more examples such as structured data parsing will be added soon.
;; Stay tuned!

(require '[mutagen.parsers.string :as string-parser])

(def GRAMMAR
  {"A" [:plus {:ok-wrapper (fn [xs] [(str (count xs) (first xs))])}
        [:char \a]]
   "B" [:plus {:ok-wrapper (fn [xs] [(str (count xs) (first xs))])}
        [:char \b]]})

(def ab-parser
  (string-parser/parser GRAMMAR [:star
                                 [:cat {:ok-wrapper (fn [[as bs]] [(str "AB: " as " " bs)])}
                                  ["A"] ["B"]]]))

(:result (ab-parser "aaabbabaaaaaab"))
;; => ["AB: 3a 2b" "AB: 1a 1b" "AB: 6a 1b"]
```

## Rationale

There are more than one good parser generator well known in clojure/java/javascript ecosystem. Some of them are limited by type of input (mostly String), obscure error messages makes impossible to analyse results of another parsers. `mutagen` is aiming to be simple but yet powerful tool to build parsers for complex data structures with enough options to produce a good error messages for better post analysis.

## Features

## API
