(ns teropa.nlp.probability.freq-dist
  "A frequency distribution for the outcomes of an experiment. A
   frequency distribution records the number of times each outcome
   of an experiment has occurred. For example, a frequency distribution
   could be used to record the frequency of each word type in a document.
   Formally, a frequency distribution can be mapped as a function mapping
   from each sample to the number of times that sample occurred as an
   outcome.
  
   Frequency distributions are generally constructed by running a 
   number of experiments, and incrementing the count for a sample
   every time it is an outcome of an experiment. For example, the
   following code will produce a frequency distribution that encodes
   how often each word occurs in a text:
   
     => (reduce
          #(incr %1 (lower %2))
          (make-freq-dist)
          sentence)")
  

(defprotocol FreqDistAccess
  (cnt [this sample]
    "Return the count of a given sample. The count of a sample is defined as
     the number of times that sample outcome was recorded by this freq dist.
     Counts are non-negative integers.")
  (n [this]
    "Return the total number of sample outcomes that have been recorded by
     this freq dist. For the number of unique sample values (or bins)
     with counts greater than zero, use b")
  (b [this]
    "Return the total number of sample values or bins that have counts greater
     than zero. For the total number of sample outcomes recorded, use n.")
  (samples [this]
    "Return a list of all samples that have been recorded as outcomes of this
     freq dist. Use cnt to determine the count of each sample")
  (hapaxes [this]
    "Return a seq of all samples that occur once (hapax legomena)")
  (nr [this r] [this r bins]
    "Return the number of samples with count r.
     bins, if given, is the number of possible sample outcomes. It is used
     to calculate (nr 0). In particular, (nr 0) is (- bins (b this)). If bins
     is not specified, it defaults to (b this), so (nr 0) will be 0") 
  (freq [this sample]
    "Return the frequencey of a given sample. The frequency of a sample is
     defined as the count of that sample divided by the total number of 
     the total number of sample outcomes that have been recorded by this
     freq dist. The count of a sample is defined as the number of times that
     sample outcome was recorded by this freq dist. Frequencies are rational
     numbers in the range of [0, 1].")
  (max-sample [this]
    "Return the sample with the greatest number of outcomes in this 
     frequency distribution. If two or more samples have the same number
     of outcomes, return one of them; which sample is returned is undefined.
     If no outcomes have occurred in this freq dist, return nil."))

(defprotocol FreqDistManipulation 
  (incr [this sample] [this sample cnt]
    "Increment this freq dist's count for the given sample")
  (set-cnt [this sample value]
    "Set this freq dist's count for the given sample."))

(defrecord FreqDist []
  
  FreqDistAccess
  (cnt [this sample]  
    (get this sample 0))
  (n [this]
    (reduce + (vals this)))
  (b [this]
    (count this))
  (samples [this]
    (keys this))
  (hapaxes [this]
    (map first (filter (fn [[k v]] (= v 1)) this)))
  (nr [this r]
    (nr this r nil))
  (nr [this r bins]
    (cond
      (neg? r)
        (throw (IndexOutOfBoundsException. "r must be non-negative"))
      (and (zero? r) (not bins))
        0
      (zero? r)
        (- bins (b this))
      :else
        (reduce
          #(if (= %2 r) (inc %1) %1)
          0
          (vals this))))
  (freq [this sample]
	  (let [n (n this)]
	    (if (zero? n)
	      0
	      (/ (cnt this sample) n))))
  (max-sample [this]
    (first
      (reduce
        (fn [[_ best-cnt :as best] [_ cand-cnt :as candidate]]
          (if (> cand-cnt best-cnt) candidate best))
        [nil 0]
        this)))
  
  FreqDistManipulation
  (incr [this sample]
    (incr this sample 1))
  (incr [this sample cnt]
    (assoc this sample
      (+ cnt (get this sample 0))))
  (set-cnt [this sample value]
    (assoc this sample value)))


(defn make-freq-dist []
  (FreqDist.))
