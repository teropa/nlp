(ns teropa.nlp.tokenizer.regexp
  (:use teropa.nlp.tokenizer)
  (:use teropa.nlp.util)
  (:import [java.util.regex Pattern]))


(defn- tokenize-with-regex
  ([text r]
    (tokenize-with-regex text r false))
  ([text r gaps]
    (tokenize-with-regex text r gaps [Pattern/MULTILINE Pattern/DOTALL]))
  ([text r gaps flags]
    (let [pattern (Pattern/compile
                    (convert-regexp-to-nongrouping r)
                    (reduce bit-or 0 flags))]
      (if gaps
        (into [] (.split pattern text))
        (re-seq pattern text)))))
      
    
(deftokenizer BlanklineTokenizer []
  (tokenize [this text]
    (tokenize-with-regex text #"\s*\n\s*\n\s*" true)))

(deftokenizer WordPunctTokenizer []
  (tokenize [this text]
    (tokenize-with-regex text #"\w+|[^\w\s]+")))
