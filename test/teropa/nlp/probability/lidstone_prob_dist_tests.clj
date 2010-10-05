(ns teropa.nlp.probability.lidstone-prob-dist-tests
  (:use clojure.test)
  (:use teropa.nlp.util)
  (:use teropa.nlp.test-util)
  (:require [teropa.nlp.probability.freq-dist :as fd])
  (:use teropa.nlp.probability.prob-dist)
  (:use teropa.nlp.probability.lidstone-prob-dist))

(let [fdist (reduce
              #(fd/incr %1 (lower %2))
              (fd/make-freq-dist)
              ["a" "b" "a" "a" "a" "c" "b" "c" "d"])
      pdist (make-lidstone-prob-dist fdist (/ 1 2))]
  
  (deftest illegal-construction-test
    (is (thrown? IllegalArgumentException
          (make-lidstone-prob-dist fdist 0.5 0)))
    (is (thrown? IllegalArgumentException
          (make-lidstone-prob-dist (fd/make-freq-dist) 0.5 nil)))
    (is (thrown? IllegalArgumentException
          (make-lidstone-prob-dist fdist 0.5 1))))
  
  (deftest probabilities-test
    (is (= (/ 9 22) (prob pdist "a")))
    (is (= (/ 5 22) (prob pdist "b")))
    (is (= -1.2895 (round (logprob pdist "a") 4)))
    (is (= -2.1375 (round (logprob pdist "b") 4))))
  
  (deftest maximum-test
    (is (= "a" (max-sample pdist))))
  
  (deftest samples-test
    (is (same-contents? ["a" "b" "c" "d"] (samples pdist))))
  
  (deftest discount-test
    (is (= (/ 2 11) (discount pdist))))
  
  (deftest generation
    (let [generated (repeatedly 10000 #(generate pdist))
          as (count (filter #{"a"} generated))
          bs (count (filter #{"b"} generated))
          cs (count (filter #{"c"} generated))
          ds (count (filter #{"d"} generated))
          others (remove #{"a" "b" "c" "d"} generated)]
      (is (> as bs))
      (is (> as cs))
      (is (> bs ds))
      (is (> cs ds))
      (is (empty? others)))))
      

(run-tests)
