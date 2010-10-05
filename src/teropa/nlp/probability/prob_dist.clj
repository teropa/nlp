(ns teropa.nlp.probability.prob-dist)

(defprotocol ProbDist
  (prob [this sample]
    "The probability of a given sample. Probabilities are always
     real numbers in the range [0, 1].")
  (logprob [this sample]
    "The base 2 logarithm of the probability of a given sample")
  (max-sample [this]
    "The sample with the greatest probability. If two or more samples have the
     same probability, return one of them; which sample is returned is undefined")
  (samples [this]
    "A list of all samples that have nonzero probabilities.")
  (discount [this]
    "The ratio by which counts are discounted on average.")
  (generate [this]
    "A randomly selected sample from this probability distribution. 
     The probability of returning each sample is equal to (prob this sample)"))
