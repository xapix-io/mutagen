(ns mutagen.grammars.json
  (:require #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
            [mutagen.core :as m]
            [mutagen.string :as sm]
            :reload-all))

(def json-grammar
  {:json           [::m/alt :null :boolean :string :number :array :object :ws]
   :null           [::m/cat {:wrap (constantly nil)}
                    [::sm/word "null"]
                    :ws]
   :boolean        [::m/cat {:wrap #(case (first %)
                                      "true"  true
                                      "false" false)}
                    [::m/alt
                     [::sm/word "false"]
                     [::sm/word "true"]]
                    :ws]
   :string         [::m/cat {:wrap #(apply str %)}
                    [::sm/char {:hide true} \"]
                    :characters
                    [::sm/char {:hide true} \"]
                    :ws]
   :characters     [::m/star :character]
   :character      [::m/alt
                    [::sm/any-char {:except [\" \\ ]}]
                    [::m/cat {:wrap (fn [[_ _ & char-code]]
                                      (char (#?(:clj Integer/parseInt :cljs js/parseInt)
                                             (apply str char-code) 16)))}
                     [::sm/char \\ ]
                     [::sm/char \u]
                     [::m/rep {:times 4} [::sm/char \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f \A \B \C \D \E \F]]]
                    [::m/cat {:wrap (fn [[_ ch]]
                                      (get {\" \"
                                            \\ \\
                                            \b \backspace
                                            \f \formfeed
                                            \n \newline
                                            \r \return
                                            \t \tab}
                                           ch))}
                     [::sm/char \\ ]
                     [::sm/char \" \\ \b \f \n \r \t]]]
   :number         [::m/cat {:wrap (fn [chs]
                                     (edn/read-string (apply str chs)))}
                    :integer [::m/opt :fraction] [::m/opt :exponent] :ws]
   :integer        [::m/cat
                    [::m/opt :sign]
                    [::m/alt
                     [::m/cat :non-zero-digit [::m/star :digit]]
                     :zero]]
   :non-zero-digit [::sm/char \1 \2 \3 \4 \5 \6 \7 \8 \9]
   :digit          [::m/alt :zero :non-zero-digit]
   :zero           [::sm/char \0]
   :fraction       [::m/cat [::sm/char \.] [::m/star :digit]]
   :sign           [::sm/char \- \+]
   :exponent       [::m/cat [::sm/char \e \E] [::m/opt :sign] :integer]
   :array          [::m/cat {:wrap (fn [x] (or x []))}
                    [::sm/char {:hide true} \[]
                    :ws
                    [::m/opt
                     [::m/cat :json [::m/star [::m/cat [::sm/char {:hide true} \,] :ws :json]]]]
                    [::sm/char {:hide true} \]]
                    :ws]
   :object         [::m/cat {:wrap #(apply hash-map %)}
                    [::sm/char {:hide true} \{]
                    :ws
                    [::m/opt
                     [::m/cat :pair [::m/star [::m/cat [::sm/char {:hide true} \,] :ws :pair]]]]
                    [::sm/char {:hide true} \}]
                    :ws]
   :pair           [::m/cat :string :ws [::sm/char {:hide true} \:] :ws :json]
   :ws             [::m/star {:hide true} [::sm/ws]]})

(def json-parser
  (m/-start-production
   (m/grammar json-grammar)
   :json))

(defn parse [st]
  (json-parser (sm/input st)))
