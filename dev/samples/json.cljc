(ns samples.json
  (:require [mutagen.parsers.string :as string-parser]
            #?(:clj [clojure.tools.reader.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])))

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

(defmulti to-json :node)

(defmethod to-json :const [{:keys [content]}]
  content)

(defmethod to-json :array [{:keys [content]}]
  (mapv to-json content))

(defmethod to-json :member [{:keys [content]}]
  (mapv to-json content))

(defmethod to-json :object [{:keys [content]}]
  (->> content
       (map to-json)
       (into {})))

(defn wrap-json [[json]]
  [{:node :json
    :content (to-json json)
    :range (:range json)}])

(def json-grammar
  {"json"
    [:alt
     ["null"]
     ["boolean"]
     ["string"]
     ["number"]
     ["object" ["member" ["string"] ["json"]]]
     ["array" ["json"]]]

    "null"
    [:cat {:ok-wrapper wrap-null}
     [:word "null"]
     ["ws"]]

    "boolean"
    [:cat {:ok-wrapper wrap-boolean}
     [:alt
      [:word "true"]
      [:word "false"]]
     ["ws"]]

    "number"
    [:cat {:ok-wrapper wrap-number}
     [:cat {:name "integer"}
      [:alt
       [:cat
        [:some-char [:range \1 \9]]
        [:star
         [:some-char [:range \0 \9]]]]
       [:char \0]]]
     [:opt
      [:cat
       [:char \.]
       [:plus [:some-char [:range \0 \9]]]]]
     [:opt
      [:cat
       [:some-char \e \E]
       [:opt
        [:some-char \- \+]]
       [:cat
        ["integer"]]]]
     ["ws"]]

    "string"
    [:cat {:ok-wrapper wrap-string}
     [:char \" ]
     [:star
      [:alt
       [:cat {:ok-wrapper wrap-escape-char}
        [:char \\ ]
        [:some-char \" \\ \b \f \n \r \t]]
       [:cat {:ok-wrapper wrap-unicode-char}
        [:char \\ ]
        [:char \u ]
        [:rep 4 [:some-char
                 [:range \0 \9]
                 [:range \A \F]
                 [:range \a \f]]]]
       [:cat
        [:neg
         [:char \" ]]
        [:any-char]]]]
     [:char \"]
     ["ws"]]

    "array"
    [:cat {:args '[itemp]
           :ok-wrapper wrap-array}
     [:char \[]
     ["ws"]
     [:alt
      [:cat
       'itemp
       [:star
        [:cat
         [:discard [:char \,]]
         ["ws"]
         'itemp]]
       [:char \]]]
      [:cat
       ["ws"]
       [:char \]]]]
     ["ws"]]

    "member"
    [:cat {:name "member"
           :args '[keyp valuep]
           :ok-wrapper wrap-member}
     'keyp
     [:discard [:char \:]]
     ["ws"]
     'valuep]

    "object"
    [:cat {:args '[member]
           :ok-wrapper wrap-object}
     [:char \{]
     ["ws"]
     [:opt
      [:cat
       'member
       [:star
        [:cat
         [:discard [:char \,]]
         ["ws"]
         'member]]]]
     [:char \}]
     ["ws"]]

   "ws"
   [:discard
    [:star
     [:some-char \space \backspace \formfeed \newline \return \tab]]]})

(def parser (string-parser/parser json-grammar [:cat ["json"] [:eof]]))

(defn parse [json-string]
  (parser json-string))
