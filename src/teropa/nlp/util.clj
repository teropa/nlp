(ns teropa.nlp.util
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.duck-streams :as stream])
  (:import [java.util Arrays]))

(defn blank-string [length]
  (let [arr (char-array length)]
    (Arrays/fill arr \space)
    (String/valueOf arr)))

(defn- wrap-idx [^String s idx]
  (if (neg? idx)
    (+ (.length s) idx)
    idx))

(defn substr
  ([^String s from]
    (.substring s (wrap-idx s from)))
  ([^String s from to]
    (.substring s (wrap-idx s from) (wrap-idx s to))))
    
(defn join-words [words]
  (string/join " " words))

(defn lower [^String s]
  (.toLowerCase s))

(defn upper [^String s]
  (.toUpperCase s))

(defn pairs [coll]
  (partition-all 2 1 coll))

(defn nonzero? [n]
  (not (zero? n)))

(defn slurp-form* [f]
  (read (stream/slurp* f)))
