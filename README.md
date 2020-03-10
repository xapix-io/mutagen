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
(require '[mutagen.grammar :as g])

(g/defgrammar KindaLispGrammar
  ;; Single whitespace character
  :ws1 [:some-char \space \backspace \formfeed \newline \return \tab]

  ;; Any whitespace character should be droped
  :ws [:skip [:star [:ws1]]]

  ;; Everything inside double quotes is a string
  :string [:cat {:wrap-res (fn [xs]
                             [(apply str (map :ch (rest (butlast xs))))])}
           [:char \"]
           [:star
            [:cat
             [:neg [:char \"]]
             [:any-char]]]
           [:char \"]
           [:ws]]

  ;; Boolean := 'true' | 'false'
  :boolean [:cat {:wrap-res (fn [xs]
                              [(if (= \t (-> xs first :ch))
                                 true
                                 false)])}
            [:alt
             [:word "true"]
             [:word "false"]]
            [:ws]]

  ;; Symbol is a sequence of more than one any character axcept whitespace, can not start with \"
  :symbol [:cat {:wrap-res (fn [xs]
                             [(symbol (apply str (map :ch xs)))])}
           [:plus
            [:cat
             [:neg [:alt [:ws1] [:char \)]]]
             [:any-char]]]
           [:ws]]

  ;; Everything inside round open and close parenthesis is a s-expression
  :s-expr [:cat {:wrap-res (fn [xs] [xs])}
           [:skip [:char \(]]
           [:ws]
           [:symbol]
           [:star [:alt
                   [:resolve [:s-expr]]
                   [:boolean]
                   [:string]]]
           [:skip [:char \)]]
           [:ws]])

(def parser (KindaLispGrammar :s-expr))

(parser "(do-something true false \"some string\" (do-something-different \"another string\" false true))")
;; => [[do-something true false "some string" [do-something-different "another string" false true]]]
```

## Rationale

## Features

## API
