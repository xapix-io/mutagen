(ns samples.json
  (:refer-clojure :exclude [array boolean])
  (:require #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
            [mutagen.grammar :as g #?@(:cljs [:include-macros true])]))

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

(g/defgrammar JSON
  :ws [:skip [:star [:some-char \space \backspace \formfeed \newline \return \tab]]]
  :null [:cat {:wrap-res wrap-null}
         [:word "null"]
         [:ws]]
  :boolean [:cat {:wrap-res wrap-boolean}
            [:alt
             [:word "true"]
             [:word "false"]]
            [:ws]]
  :non-zero-digit [:some-char [:range \1 \9]]
  :digit [:alt
          [:char \0]
          [:non-zero-digit]]
  :integer [:alt
            [:cat
             [:non-zero-digit]
             [:star [:digit]]]
            [:char \0]]
  :number [:cat {:wrap-res wrap-number}
           [:integer]
           [:opt
            [:cat [:char \.] [:plus [:digit]]]]
           [:opt
            [:cat
             [:some-char \e \E]
             [:opt
              [:some-char \- \+]]
             [:integer]]]
           [:ws]]
  :string [:cat {:wrap-res wrap-string}
           [:char \"]
           [:star
            [:alt
             [:cat {:wrap-res wrap-escape-char}
              [:char \\ ]
              [:some-char \" \\ \b \f \n \r \t]]
             [:cat {:wrap-res wrap-unicode-char}
              [:char \\ ]
              [:char \u]
              [:rep 4 [:some-char [:range \0 \9] [:range \a \f] [:range \A \F]]]]
             [:cat
              [:neg [:char \"]]
              [:any-char]]]]
           [:char \"]
           [:ws]]
  :array [:cat {:wrap-res wrap-array}
          [:char \[]
          [:ws]
          [:alt
           [:cat
            [:resolve [:json]]
            [:star [:cat [:skip [:char \,]] [:ws] [:resolve [:json]]]]
            [:char \]]]
           [:char \]]]
          [:ws]]
  :pair [:cat {:wrap-res wrap-pair}
         [:string]
         [:skip [:char \:]]
         [:ws]
         [:resolve [:json]]]
  :object [:cat {:wrap-res wrap-object}
           [:char \{]
           [:ws]
           [:alt
            [:cat
             [:pair]
             [:star [:cat [:skip [:char \,]] [:ws] [:pair]]]
             [:char \}]]
            [:char \}]]
           [:ws]]
  :json [:alt
         [:null]
         [:boolean]
         [:string]
         [:number]
         [:array]
         [:object]]
  :json-document [:cat
                  [:ws]
                  [:alt
                   [:json]
                   [:eps]]
                  [:eof]]
  :json-documents [:cat
                   [:ws]
                   [:plus [:json]]
                   [:eof]])

(def parse (JSON :json-document))

(comment

  (parse "[1,2,3]")

  )
