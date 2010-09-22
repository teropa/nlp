(ns teropa.nlp.metrics.score
  (:require [clojure.set :as set]))


(defn accuracy
  "Given a seq of reference values and a corresponding seq of test
   values, return the fraction of corresponding values that are equal.
   In particular, return the fraction of indices
   0 < i <= len(test), such that test[i] == reference[i]."
  [reference test]
  (when-not (= (count reference) (count test))
    (throw (IllegalArgumentException. "lists must have the same length")))
  (reduce
    (fn [res [lhs rhs]] (if (= lhs rhs) (inc res) res))
    0
    (map list reference test)))

(defn precision
  "Given a set of reference values and a set of test values, return
   the fraction of test values that appear in the reference set."
  [reference test]
  (when-not (and (set? reference) (set? test))
    (throw (IllegalArgumentException. "arguments must be sets")))
  (/ (count (set/intersection reference test))
     (count test)))

(defn recall
  "Given a set of reference values and a set of test values, return
   the fraction of reference values that appear in the test set."
  [reference test]
  (when-not (and (set? reference) (set? test))
    (throw (IllegalArgumentException. "arguments must be sets")))
  (/ (count (set/intersection reference test))
     (count reference)))
  

(defn f-measure 
  "Given a set of reference values and a set of test values, return
   the f-measure of the test values, when compared against the
   reference values. The f-measure is the harmonic mean of the
   precision and recall, weighted by alpha. In particular, given
   the precision p and recall r defined by:"
  ([reference test] (f-measure reference test 0.5))
  ([reference test alpha]
    (let [p (precision reference test)
          r (recall reference test)]
      (if (or (zero? p) (zero? r))
        0
        (/ 1.0
           (+ (/ alpha p)
              (/ (- 1 alpha)
                 r)))))))
