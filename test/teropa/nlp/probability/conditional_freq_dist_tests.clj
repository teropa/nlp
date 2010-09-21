(ns teropa.nlp.probability.freq-dist-tests
  (:use clojure.test)
  (:use teropa.nlp.probability.conditional-freq-dist)
  (:require [teropa.nlp.probability.freq-dist :as fdist])
  (:use teropa.nlp.util))

(deftest freq-dist-tests
  (let [f (reduce
            (fn [cfdist word]
              (let [condition (.length word)]
                (incr cfdist condition word)))
            (make-conditional-freq-dist)
            ["a" "a" "b"
             "aa" "ab" "bb"
             "aaa" "aab" "aba" "baa" "abb" "bab" "bba" "bbb" "bbb" "bbb"])]
    
    (is (= #{1 2 3} (apply hash-set (conditions f))))
    (is (= 16 (n f)))
    (is (= 3 (fdist/n (dist f 1))))
    (is (= 2 (fdist/b (dist f 1))))))






(run-tests)
 