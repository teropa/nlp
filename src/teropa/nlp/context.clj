(ns teropa.nlp.context
  (:require [clojure.set :as set])
  (:use teropa.nlp.util)
  (:require [teropa.nlp.probability.conditional-freq-dist :as cfd])
  (:require [teropa.nlp.probability.freq-dist :as fd])
  (:require [teropa.nlp.metrics.score :as score]))

(defprotocol Context
  (word-similarity-map [this word]
    "Return a similarity mapping from words to 'similarity scores,'
     indicating how often these two words occur in the same context")
  (similar-words [this word] [this word n])
  (common-contexts [this words] [this words fail-on-unknown]
    "Find contexts where the specified words can all appear; and
     return a frequency distribution mapping each context to the
     number of times that context was used"))

(defrecord ContextIndex [word-to-contexts context-to-words key-fn]
  Context
  (word-similarity-map [this word]
    (let [contexts (as-set (cfd/dist word-to-contexts (key-fn word)))]
      (reduce
        (fn [scores [w w-contexts]]
          (assoc scores w (score/f-measure contexts (as-set w-contexts))))
        {}
        word-to-contexts)))
  (similar-words [this word]
    (similar-words this word 20))
  (similar-words [this word n]
    (let [scores (for [c (cfd/samples word-to-contexts (key-fn word))
                       w (cfd/samples context-to-words c) :when (not= w word)]
                   {:word w 
                    :score (* (cfd/freq context-to-words c word)
                              (cfd/freq context-to-words c w))})
          totals-by-word (map 
                           (fn [[word entries]]
                             [word (reduce + (map :score entries))])
                           (group-by :word scores))]
      (->> totals-by-word
           (sort-by second)
           reverse
           (take n)
           (map first))))
  (common-contexts [this words]
    (common-contexts this words false))
  (common-contexts [this words fail-on-unknown]
    (let [words (map key-fn words)
          contexts (map #(as-set (cfd/samples word-to-contexts %)) words)
          common (reduce set/intersection contexts)]
      (if (and fail-on-unknown (some empty? contexts))
        (throw (IllegalArgumentException. (str "The following word(s) were not found: " words)))
        (reduce fd/incr
                (fd/make-freq-dist)
                (filter common (apply concat contexts)))))))

(defn- default-context-fn [tokens i]
  [(if (zero? i) 
     "*START*"
     (lower (nth tokens (dec i))))
   (if (= i (dec (count tokens)))
     "*END*"
     (lower (nth tokens (inc i))))])

(defn make-context-index
  ([tokens]
    (make-context-index tokens default-context-fn))
  ([tokens context-fn]
    (make-context-index tokens context-fn lower))
  ([tokens context-fn key-fn]
    (let [tokens (as-vec tokens)
          tokens-with-contexts
            (map
              (fn [[idx token]]
                [token (context-fn tokens idx)])
              (indexed tokens))
          [word-to-contexts context-to-words]
            (reduce
              (fn [[wtc ctw] [token context]]
                (let [k (key-fn token)]
                  [(cfd/incr wtc k context)
                   (cfd/incr ctw context k)]))
              [(cfd/make-conditional-freq-dist)
               (cfd/make-conditional-freq-dist)]
              tokens-with-contexts)]
      (ContextIndex.
        word-to-contexts
        context-to-words
        key-fn))))
