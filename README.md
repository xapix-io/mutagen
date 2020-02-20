# mutagen

> Yet another LL(k) parser generator.

---

> Please note that this is a beta version of the `mutagen` library which is
> still undergoing final testing before its official release. The
> library, its software and all content found on it are provided on an
> “as is” and “as available” basis. `mutagen` does not give any warranties,
> whether express or implied, as to the suitability or usability of the
> library, its software or any of its content.
>
> `mutagen` will not be liable for any loss, whether such loss is direct,
> indirect, special or consequential, suffered by any party as a result
> of their use of the `mutagen` library, its software or content.
>
> Should you encounter any bugs, glitches, lack of functionality or
> other problems with the library, please let us know immediately so we
> can rectify these accordingly. Your help in this regard is greatly
> appreciated.

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
