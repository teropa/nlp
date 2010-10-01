(ns teropa.nlp.probability.conditional-freq-dist
  "A collection of frequency distributions for a single experiment
   run under different conditions. Conditional frequency
   distributions are used to record the number of times each sample
   occurred, given the condition under which the experiment was run.
   For example, a conditional frequency distribution could be used to
   record the frequency of each word (type) in a document, given its
   length. Formally, a conditional frequency distribution can be
   defined as a functionthat maps from each condition to the FreqDist
   for the experiment under that condition.
  
   The frequency distribution for each condition is accessed using
   the cfdist-dist function:
  
     =>  (cfdist-dist cfdist 3)
  
   When the get-freq-dist operator is used to access the frequency
   distribution for a condition that has not been accessed before,
   conditional-freq-dist returns a new empty freq-dist for that
   condition. 
  
   Conditional frequency distributions are typically constructed by
   repeatedly running an experiment under a variety of conditions,
   and incrementing the sample outcome counts for the appropriate
   conditions. For example, the following code will produce a
   conditional frequency distribution that encodes how often each
   word type occurs, given the length of that word type:
  
     => (reduce
          (fn [cfdist word]
            (let [condition (.length word)]
               (incr cfdist condition word)))
          (make-conditional-freq-dist)
          sentence)"
  (:require [teropa.nlp.probability.freq-dist :as fdist]))

(defprotocol ConditionalFreqDistAccess
  (dist [this condition])
  (samples [this condition])
  (freq [this condition sample])
  (conditions [this])
  (n [this]))

(defprotocol ConditionalFreqDistManipulation
  (incr [this condition sample] [this condition sample cnt]
    "Increment this freq dist's count for the given condition and sample")
  (set-cnt [this condition sample value]
    "Set this freq dist's count for the given condition and sample."))

(defrecord ConditionalFreqDist []
  
  ConditionalFreqDistAccess
  (dist [this condition]
    (get this condition (fdist/make-freq-dist)))
  (freq [this condition sample]
    (fdist/freq (dist this condition) sample))
  (samples [this condition]
    (fdist/samples (dist this condition)))
  (conditions [this]
    (sort (keys this)))
  (n [this]
    (reduce + (map fdist/n (vals this))))
  
  ConditionalFreqDistManipulation
  (incr [this condition sample]
    (incr this condition sample 1))
  (incr [this condition sample cnt]
    (let [d (dist this condition)]
      (assoc this condition
        (fdist/incr d sample cnt))))
  (set-cnt [this condition sample cnt]
    (let [d (dist this condition)]
      (assoc this condition
        (fdist/set-cnt d sample cnt)))))

(defn make-conditional-freq-dist []
  (ConditionalFreqDist.))

