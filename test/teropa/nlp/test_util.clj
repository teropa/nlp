(ns teropa.nlp.test-util
  (:require [clojure.set :as set])
  (:use teropa.nlp.util))

(defn same-contents?
  "Whether all the given collections have the same
   items (not necessarily in the same order)"
  [& colls]
  (= (count (first colls))
     (count (reduce
              set/intersection
              (map as-set colls)))))
