(ns dev
  )

(def G
  {"X" [:alt
        [:char \q]
        [:char \w]
        [:char \e]]})

(comment

  (let [p (string-parser/parser G ["X"])]
    (p "r"))

  (use 'blancas.kern.core
       'blancas.kern.lexer.basic)

  (declare json)

  (def pair (bind [f string-lit _ colon v json]
                  (return [f v])) )

  (def array (brackets (comma-sep (fwd json))))

  (def object (braces
               (bind [fields (comma-sep pair)]
                     (return (apply hash-map (reduce concat [] fields))))))

  (def json (<|> string-lit dec-lit float-lit object array bool-lit nil-lit) )

  )
