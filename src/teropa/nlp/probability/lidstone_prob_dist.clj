(ns teropa.nlp.probability.lidstone-prob-dist
  "The Lidstone estimate for the probability distribution of the
   experiment used to generate a frequency distribution. The
   Lidstone estimate is parameterized by a real number gamma,
   which typically ranges from 0 to 1. The Lidstone estimate
   approximates the probability of a sample with count c from
   an experiment with N outcomes and B bins as (c+gamma)/(N+B*gamma).
   This is equivalent to adding gamma to the count for each bin,
   and taking the maximum likelihood estimate of the resulting
   frequency distribution"
  (:use teropa.nlp.util)
  (:require [teropa.nlp.probability.freq-dist :as fd])
  (:require [teropa.nlp.probability.prob-dist :as pd]))

(defrecord LidstoneProbDist [freqdist gamma n bins divisor]
  pd/ProbDist
  (prob [this sample]
    (let [c (fd/cnt freqdist sample)]
      (/ (+ c gamma)
         divisor)))
  (logprob [this sample]
    (log2 (pd/prob this sample)))
  (max-sample [this]
    (fd/max-sample freqdist))
  (samples [this]
    (fd/samples freqdist))
  (discount [this]
    (let [gb (* gamma bins)]
      (/ gb (+ n gb))))
  (generate [this]
      (loop [p (rand),
             [sample & rst] (pd/samples this)]
        (cond
          sample
            (let [nextp (- p (pd/prob this sample))]
              (if (<= nextp 0)
                sample
                (recur nextp rst)))
          (< p 0.0001) ; allow for some rounding error
            sample
          :else ; we *should* never get here
            (rand-nth (pd/samples this))))))
            

(defn- assert-bins [bins freqdist]
  (if (or (and bins (zero? bins))
          (and (nil? bins) (zero? (fd/n freqdist))))
      (throw-illegal-arg "A lidstone probability distribution must have at least one bin"))
  (if (and bins (< bins (fd/b freqdist)))
      (throw-illegal-arg "The number of bins in a lidstone probability distribution ("
                         bins ") must be greater than or equal to the number of bins "
                         " in the freqdist used to create it (" (fd/n freqdist) ")")))
  
(defn make-lidstone-prob-dist
  "Use the Lidstone estimate to create a probability distribution
   for the experiment used to generate freqdist."
  ([freqdist gamma] (make-lidstone-prob-dist freqdist gamma nil))
  ([freqdist gamma bins]
    (assert-bins bins freqdist)
    (let [bins (or bins (fd/b freqdist))
          divisor (+ (fd/n freqdist)
                     (* bins gamma))
          gamma (if (zero? divisor) 0 gamma)
          divisor (if (zero? divisor) 1 divisor)]
      (LidstoneProbDist.
        freqdist
        gamma
        (fd/n freqdist)
        bins
        divisor))))
      


