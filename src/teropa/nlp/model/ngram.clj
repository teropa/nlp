(ns teropa.nlp.model.ngram
  "A processing interface for assigning a probability to the next word."
  (:use teropa.nlp.util)
  (:require [teropa.nlp.probability.conditional-freq-dist :as cfd])
  (:require [teropa.nlp.probability.conditional-prob-dist :as cpd])
  (:require [teropa.nlp.probability.prob-dist :as pd])
  (:require [teropa.nlp.probability.lidstone-prob-dist :as lpd]))

(defn- beta [ngram tokens]
  (if (some= tokens (cpd/conditions (:model ngram)))
      (pd/discount (cpd/dist (:model ngram) tokens))
      1))

(defn- generate-one [{:keys [prefix n model backoff]} context]
  (let [context (vec (take-last (dec n)
                                (concat prefix context)))]
    (cond
      (cpd/contains model context)
        (pd/generate (cpd/dist model context))
      (> n 1)
        (generate-one backoff (rest context))
      :else
        ".")))
  
(defn- alpha [ngram tokens]
  (/ (beta ngram tokens)
     (beta (:backoff ngram) (rest tokens))))

(defprotocol Ngram
  (prob [this word context]
    "Evaluate the probability of this word in context")
  (logprob [this word context])
  (generate [this num-words] [this num-words context])
  (entropy [this text]))

(defrecord NgramModel [n ngrams prefix model backoff]
  Ngram
  (prob [this word context]
    (let [context (vec context)]
      (cond
        (contains? ngrams (conj context word))
          (pd/prob (cpd/dist model context) word)
        (> n 1)
          (* (alpha this context)
            (prob backoff word (rest context)))
        :else
          (throw-illegal-arg "No probability mass assigned to word" word "in context" context)))) 
  (logprob [this word context]
    (- (log2 (prob this word context))))
  (generate [this num-words]
    (generate this num-words []))
  (generate [this num-words context]
    (reduce
      (fn [text _]
        (conj text (generate-one this text)))
      []
      (range num-words))))
      
(defn make-ngram-model
  "Creates an ngram language model to capture patterns in n consecutive
   words of training text. An estimator smooths the probabilities derived
   from the text and may allow generation of ngrams not seen during
   training."
  ([n train] 
    (make-ngram-model n train (fn [fdist _] (lpd/make-lidstone-prob-dist fdist (/ 1 5)))))
  ([n train estimator]
    (let [prefix (repeat (dec n) "")
          ngrams (ingrams (concat prefix train) n)
          cfdist (->> ngrams
                      (map #(split-at (dec n) %))
                      (reduce
                        (fn [cfdist [context [token]]]
                          (cfd/incr cfdist (vec context) token))
                        (cfd/make-conditional-freq-dist)))
          model (cpd/make-conditional-prob-dist cfdist estimator (cfd/n cfdist))
          backoff (if (> n 1) (make-ngram-model (dec n) train estimator))]
      (NgramModel. n (as-set ngrams) prefix model backoff))))
    
    
  
  