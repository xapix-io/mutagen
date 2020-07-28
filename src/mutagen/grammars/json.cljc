(ns mutagen.grammars.json
  (:require #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
            [mutagen.core :as m]))

(def json-grammar
  {:document       [::m/cat [::m/star :json] ::m/eof]
   :json           [::m/alt :null :boolean :string :number :array :object]
   :null           [::m/cat {:wrap (constantly nil)}
                    [::m/word "null"]
                    :ws]
   :boolean        [::m/cat {:wrap #(case (first %)
                                      \t true
                                      \f false)}
                    [::m/alt
                     [::m/word "false"]
                     [::m/word "true"]]
                    :ws]
   :string         [::m/cat {:wrap #(apply str %)}
                    [::m/char {:hide true} \"]
                    :characters
                    [::m/char {:hide true} \"]
                    :ws]
   :characters     [::m/star :character]
   :character      [::m/alt
                    [::m/any-char {:except [\" \\ ]}]
                    [::m/cat {:wrap (fn [[_ _ & char-code]]
                                      (char (#?(:clj Integer/parseInt :cljs js/parseInt)
                                             (apply str char-code) 16)))}
                     [::m/char \\ ]
                     [::m/char \u]
                     [::m/rep {:times 4} [::m/char \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f \A \B \C \D \E \F]]]
                    [::m/cat {:wrap (fn [[_ ch]]
                                      (get {\" \"
                                            \\ \\
                                            \b \backspace
                                            \f \formfeed
                                            \n \newline
                                            \r \return
                                            \t \tab}
                                           ch))}
                     [::m/char \\ ]
                     [::m/char \" \\ \b \f \n \r \t]]]
   :number         [::m/cat {:wrap (fn [chs]
                                     (edn/read-string (apply str chs)))}
                    :integer [::m/opt :fraction] [::m/opt :exponent] :ws]
   :integer        [::m/cat
                    [::m/opt :sign]
                    [::m/alt
                     [::m/cat :non-zero-digit [::m/star :digit]]
                     :zero]]
   :non-zero-digit [::m/char \1 \2 \3 \4 \5 \6 \7 \8 \9]
   :digit          [::m/alt :zero :non-zero-digit]
   :zero           [::m/char \0]
   :fraction       [::m/cat [::m/char \.] [::m/star :digit]]
   :sign           [::m/char \- \+]
   :exponent       [::m/cat [::m/char \e \E] [::m/opt :sign] :integer]
   :array          [::m/alt
                    :e-array
                    :ne-array]
   :e-array        [::m/cat {:wrap (constantly [])}
                    [::m/char {:hide true} \[]
                    :ws
                    [::m/char {:hide true} \]]
                    :ws]
   :ne-array       [::m/cat {:wrap vec}
                    [::m/char {:hide true} \[]
                    :ws
                    [::m/cat :json [::m/star [::m/cat [::m/char {:hide true} \,] :ws :json]]]
                    [::m/char {:hide true} \]]
                    :ws]
   :object         [::m/alt
                    :e-object
                    :ne-object]
   :e-object       [::m/cat {:wrap (constantly {})}
                    [::m/char {:hide true} \{]
                    :ws
                    [::m/char {:hide true} \}]
                    :ws]
   :ne-object      [::m/cat {:wrap #(do (prn %) (apply hash-map %))}
                    [::m/char {:hide true} \{]
                    :ws
                    [::m/cat :pair [::m/star [::m/cat [::m/char {:hide true} \,] :ws :pair]]]
                    [::m/char {:hide true} \}]
                    :ws]
   :pair           [::m/cat :string :ws [::m/char {:hide true} \:] :ws :json]
   :ws             [::m/star {:hide true} ::m/ws]})

(def json-parser
  (m/-parser
   (m/grammar json-grammar)
   :document))

(defn parse [st]
  (json-parser st []))
