(ns teropa.nlp.concordance
  (:require [clojure.contrib.string :as string])
  (:require [teropa.nlp.corpus.corpus-reader :as c])
  (:use teropa.nlp.util))

(defn- print-conc [concordance]
  (println "Displaying" (count concordance) "matches")
  (doseq [c concordance]
    (println c)))

(defprotocol Concordance
  (concordance [this word] [this word width] [this word width lines])
  (print-concordance [this word] [this word width] [this word width lines])
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
            (let [lhs (str (blank-string half-width)
                           (join-words (subvec tokens (- offset context) offset)))
                  rhs (join-words (subvec tokens (inc offset) (+ offset context)))]
              (str
                (substr lhs (- half-width))
                " "
                word
                " "
                (substr rhs 0 half-width))))
          (take line-count offsts)))))
  (print-concordance [this word]
    (print-conc (concordance this word)))
  (print-concordance [this word width]
    (print-conc (concordance this word width)))
  (print-concordance [this word width line-count]
    (print-conc (concordance this word width line-count))))
  
  
(defn make-index
  ([words] (make-index words identity))
  ([words key-fn]
    (let [word-vec (into [] words)]
      (ConcordanceIndex.
        word-vec
        (reduce
          (fn [res [idx word]]
            (let [key (key-fn word)]
              (if (res key)
                (update-in res [key] conj idx)
                (assoc res key [idx]))))
          {}
          (indexed word-vec))
        key-fn))))

    
 