(ns teropa.nlp.model.ngram-tests
  (:use clojure.test)
  (:use teropa.nlp.model.ngram))

(let [ngram (make-ngram-model ["easter" "bunny" "easter" "fox" "easter" "bunny"] 2)]
  
  (deftest probabilities
    (is (= (/ 11 17) (prob ngram "bunny" ["easter"])))
    (is (= (/ 6 17) (prob ngram "fox" ["easter"]))))
  
  (println (generate ngram 10)))


(run-tests)
