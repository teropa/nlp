(ns teropa.nlp.probability.conditional-prob-dist-tests
  (:use clojure.test)
  (:use teropa.nlp.test-util)
  (:require [teropa.nlp.probability.conditional-freq-dist :as cfd])
  (:require [teropa.nlp.probability.prob-dist :as pd])
  (:require [teropa.nlp.probability.lidstone-prob-dist :as lpd])
  (:use teropa.nlp.probability.conditional-prob-dist))

(let [cfdist (reduce
               (fn [res word]
                 (let [condition (.length word)]
                   (cfd/incr res condition word)))
               (cfd/make-conditional-freq-dist)
               ["a" "a" "b"
                "aa" "ab" "bb"
                "aaa" "aab" "aba" "baa" "abb" "bab" "bba" "bbb" "bbb" "bbb"])
      cpdist (make-conditional-prob-dist cfdist lpd/make-lidstone-prob-dist (/ 1 2))]
  
  (deftest conditions-test
    (is (= (same-contents? [1 2 3] (conditions cpdist))))
    (is (= 3 (cnt cpdist)))
    (is (contains cpdist 3))
    (is (= "a" (pd/max-sample (dist cpdist 1))))))



(run-tests)
