(ns teropa.nlp.probability.freq-dist-tests
  (:use clojure.test)
  (:use teropa.nlp.probability.freq-dist)
  (:use teropa.nlp.util))

(deftest freq-dist-tests
  (let [f (reduce
            #(incr %1 (lower %2))
            (make-freq-dist)
            ["a" "b" "a" "a" "a" "c" "b" "c" "d"])]
    (is (= 4 (cnt f "a")))
    (is (= 2 (cnt f "c")))
    (is (= 9 (n f)))
    (is (= 4 (b f)))
    (is (= #{"a" "b" "c" "d"} (apply hash-set (samples f))))
    (is (= ["d"] (hapaxes f)))
    (is (= 2 (nr f 2)))
    (is (= 1 (nr f 4)))
    (is (= 0 (nr f 0)))
    (is (= 1 (nr f 0 5)))
    (is (= (/ 4 9) (freq f "a")))
    (is (= "a" (max-sample f)))))

(run-tests)
 