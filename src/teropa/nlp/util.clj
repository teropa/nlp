(ns teropa.nlp.util
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.duck-streams :as stream])
  (:import [java.io File])
  (:import [java.util Arrays])
  (:import [java.util.regex Pattern])
  (:import [org.apache.commons.io FileUtils])
  (:import [org.apache.commons.io.filefilter RegexFileFilter TrueFileFilter]))

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
  (read-string (stream/slurp* f)))

(defn re-sub [pattern replacement-fn s]
  (let [matcher (.matcher pattern s)
        result (StringBuffer.)]
    (while (.find matcher)
      (let [replacement (replacement-fn (.group matcher))
            escaped-replacement (.replaceAll replacement "\\\\" "\\\\\\\\")]
          (.appendReplacement matcher result escaped-replacement)))
    (.appendTail matcher result)
    (.toString result)))

(defn convert-regexp-to-nongrouping [p]
  (let [p (if (instance? Pattern p) (.pattern p) p)]
    (if (re-matches #"\\[0-9]" p)
      (throw (UnsupportedOperationException. "Regular expressions with back-references are not supported")))
    (re-sub
      #"\\.|\(\?|\("
      #(.. #"^\($" (matcher %) (replaceAll "(?:"))
      p)))

(defn filenames-by-regex [root-file regex]
  (let [pattern (if (instance? Pattern regex) regex (Pattern/compile regex))
        root-path-length (inc (.length (.getAbsolutePath root-file)))]
    (apply sorted-set
      (map
        #(.substring (.getAbsolutePath %) root-path-length)
        (FileUtils/listFiles
          root-file
          (RegexFileFilter. pattern)
          TrueFileFilter/INSTANCE)))))

(defn slurp-all [files]
  (reduce str (map stream/slurp* files)))

(defn indexed [coll]
  (map list (iterate inc 0) coll))
