(ns teropa.nlp.concordance
  (:require [clojure.contrib.string :as string])
  (:use teropa.nlp.util))

(defprotocol Concordance
  (concordance [this word] [this word width] [this word width lines])
  (offsets [this word]))

(defrecord ConcordanceIndex [tokens the-index key-fn]
  Concordance
  (offsets [this word]
    (the-index (key-fn word)))
  (concordance [this word] (concordance this word 75))
  (concordance [this word width] (concordance this word width 25))
  (concordance [this word width line-count]
    (when-let [offsts (offsets this word)]
      (let [half-width (/ (- width (.length word)) 2)
            context (/ width 4)] ; approx number of words of context
        (map
          (fn [offset]
            (let [lhs (str
                        (blank-string half-width)
                        (join-words (subvec tokens (- offset context) offset)))
                  rhs (join-words (subvec tokens (inc offset) (+ offset context)))]
              (str
                (substr lhs (- half-width))
                word
                (substr rhs 0 half-width))))
          (take line-count offsts))))))
  
  
(defn make-index
  ([text] (make-index text identity))
  ([text key-fn]
    (ConcordanceIndex.
      (:tokens text)
      (reduce
        (fn [res [idx word]]
          (update-in res [(key-fn word)] conj idx))
        {}
        (:tokens text))
      key-fn)))

    
 