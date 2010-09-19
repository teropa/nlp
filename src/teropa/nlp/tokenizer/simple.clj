(ns teropa.nlp.tokenizer.simple
  (:require [clojure.contrib.string :as string])
  (:require [teropa.nlp.tokenizer :as tokenizer]))

(deftype WhitespaceTokenizer []
  tokenizer/Tokenizer
  (tokenize [this s]
    (vec (.split #"\s" s)))
  (batch-tokenize [this s]
    (throw (UnsupportedOperationException. "WhitespaceTokenizer does not support batch tokenizing"))))
    
(deftype SpaceTokenizer []
  tokenizer/Tokenizer
  (tokenize [this s]
    (vec (.split #" " s)))
  (batch-tokenize [this s]
    (throw (UnsupportedOperationException. "SpaceTokenizer does not support batch tokenizing"))))

(deftype TabTokenizer []
  tokenizer/Tokenizer
  (tokenize [this s]
    (vec (.split #"\t" s)))
  (batch-tokenize [this s]
    (throw (UnsupportedOperationException. "TabTokenizer does not support batch tokenizing"))))

(deftype CharTokenizer []
  tokenizer/Tokenizer
  (tokenize [this s]
    (vec (drop 1 (.split #"" s))))
  (batch-tokenize [this s]
    (throw (UnsupportedOperationException. "CharTokenizer does not support batch tokenizing"))))

(deftype LineTokenizer [blank-lines]
  tokenizer/Tokenizer
  (tokenize [this s]
    (let [lines (.split #"\n" s)]
      (condp = blank-lines
        :discard
          (vec (remove string/blank? lines))
        :discard-eof
          (let [lines-vec (vec lines)]
            (if (string/blank? (peek lines-vec))
              (pop lines-vec)
              lines-vec))
        (vec lines))))
  (batch-tokenize [this s]
    (throw (UnsupportedOperationException. "CharTokenizer does not support batch tokenizing"))))
