(ns teropa.nlp.probability.conditional-prob-dist
  "A conditional probability distribution modelling the experiments
   that were used to generate a conditional frequency distribution."
  (:require [teropa.nlp.probability.conditional-freq-dist :as cfd])
  (:require [teropa.nlp.probability.freq-dist :as fd]))

(defprotocol ConditionalProbDistAccess
  (contains [this condition])
  (dist [this condition])
  (conditions [this])
  (cnt [this]))

(defrecord ConditionalProbDist [pdists probdist-factory factory-args]
  ConditionalProbDistAccess
  (contains [this condition]
    (contains? pdists condition))
  (dist [this condition]
    (if (contains this condition)
        (pdists condition)
        (apply probdist-factory (fd/make-freq-dist) factory-args)))
  (conditions [this]
    (keys pdists))
  (cnt [this]
    (count pdists)))
      

(defn make-conditional-prob-dist
  "Construct a new conditional probability distribution, based on
   the given conditional frequency distribution and prob dist factory"
  [cfdist probdist-factory & factory-args]
  (ConditionalProbDist.
    (reduce
      (fn [res c]
        (assoc res c (apply probdist-factory (cfd/dist cfdist c) factory-args)))
      {}
      (cfd/conditions cfdist))
    probdist-factory
    factory-args))
