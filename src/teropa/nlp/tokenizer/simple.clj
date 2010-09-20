(ns teropa.nlp.tokenizer.simple
  (:require [clojure.contrib.string :as string])
  (:use teropa.nlp.tokenizer))

(deftokenizer WhitespaceTokenizer []
  (tokenize [this s] (tokenize this s nil))
  (tokenize [this s args]
    (vec (.split #"\s" s))))
    
(deftokenizer SpaceTokenizer []
  (tokenize [this s] (tokenize this s nil))
  (tokenize [this s args]
    (vec (.split #" " s))))

(deftokenizer TabTokenizer []
  (tokenize [this s] (tokenize this s nil))
  (tokenize [this s args]
    (vec (.split #"\t" s))))

(deftokenizer CharTokenizer []
  (tokenize [this s] (tokenize this s nil))
  (tokenize [this s args]
    (vec (drop 1 (.split #"" s)))))

(deftokenizer LineTokenizer [blank-lines]
  (tokenize [this s] (tokenize this s nil))
  (tokenize [this s args]
    (let [lines (.split #"\n" s)]
      (condp = blank-lines
        :discard
          (vec (remove string/blank? lines))
        :discard-eof
          (let [lines-vec (vec lines)]
            (if (string/blank? (peek lines-vec))
              (pop lines-vec)
              lines-vec))
        (vec lines)))))
