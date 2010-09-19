(ns teropa.nlp.util-tests
  (:use clojure.test)
  (:use teropa.nlp.util))

(deftest string-convenience-funcs
  
  (is (= "    " (blank-string 4)))
  (is (= ""     (blank-string 0)))
  
  (is (= "ello" (substr "hello" 1)))
  (is (= "hell" (substr "hello" 0  4)))
  (is (= "lo"   (substr "hello" -2)))
  (is (= "ell"  (substr "hello" 1  -1)))
  
  (is (= "have a nice day" (join-words ["have" "a" "nice" "day"]))))

(run-tests)
