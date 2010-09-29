(ns teropa.nlp.context
  (:use teropa.nlp.util)
  (:require [teropa.nlp.probability.conditional-freq-dist :as cfd])
  (:require [teropa.nlp.metrics.score :as score]))

(defn- default-context-fn [tokens i]
  [(if (zero? i) 
     "*START*"
     (lower (nth tokens (dec i))))
   (if (= i (dec (count tokens)))
     "*END*"
     (lower (nth tokens (inc i))))])

(defprotocol Context
  (word-similarity-map [this word]
    "Return a similarity mapping from words to 'similarity scores,'
     indicating how often these two words occur in the same context")
  (similar-words [this word] [this word n]))

(defrecord ContextIndex [word-to-contexts context-to-words key-fn]
  Context
  (word-similarity-map [this word]
    (let [contexts (as-set (cfd/dist word-to-contexts (key-fn word)))]
      (reduce
        (fn [scores [w w-contexts]]
          (assoc scores w (score/f-measure contexts (as-set w-contexts))))
        {}
        word-to-contexts))))


(defn make-context-index
  ([tokens]
    (make-context-index tokens default-context-fn))
  ([tokens context-fn]
    (make-context-index tokens context-fn identity))
  ([tokens context-fn key-fn]
    (let [tokens-with-contexts
          (map
            (fn [[idx token]]
              [token (context-fn tokens idx)])
            (indexed tokens))
          [word-to-contexts context-to-words]
            (reduce
              (fn [[wtc ctw] [token context]]
                (let [k (key-fn token)]
                  (println k)
                  [(cfd/incr wtc k context)
                   (cfd/incr ctw context k)]))
              [(cfd/make-conditional-freq-dist)
               (cfd/make-conditional-freq-dist)]
              tokens-with-contexts)]
      (ContextIndex.
        word-to-contexts
        context-to-words
        lower))))

