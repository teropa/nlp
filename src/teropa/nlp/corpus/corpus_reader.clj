(ns teropa.nlp.corpus.corpus-reader
  (:require [clojure.string :as string])
  (:require [clojure.contrib.duck-streams :as streams])
  (:import [java.io File])
  (:import [org.apache.commons.io FilenameUtils]))

(defprotocol CorpusReader
  (readme [this]
    "Returns the contents of the corpus README file, if it exists.")
  (abspath [this fileid]
    "Return the absolute path for the given file")
  (abspaths [this] [this fileids] [this fileids include-encoding]
    "Returns a seq of the absolute paths for all fileids in this corpus,
     or the list of given fileids. if include-encoding is true, returns
     a list of [path encoding] vectors")
  (open [this file]
    "Returns an open reader that can be used to read the given file.")
  (encoding [this file]
    "Return the unicode encoding of the given corpus file, if known")
  (raw [this] [this fileids]
    "Return the given files as a single string")
  (words [this] [this fileids]
    "Return the given files as a seq of words")
  (sents [this] [this fileids]
    "Return the given files as a seq of sentences or utterances,
     each encoded as a seq of word strings")
  (paras [this] [this fileids]
    "Return the given files as a seq of paragraphs, each encoded as a seq
     of sentences, which are in turn encoded as seqs of word strings"))


(def default-impls
  {:encoding
     (fn [this file]
       (if (associative? (:encoding this))
         (get (:encoding this) file)
         (:encoding this)))
   :open
     (fn [this file]
       (let [encoding (encoding this file)]
         (binding [streams/*default-encoding* encoding]
           (streams/reader (FilenameUtils/concat (:root this) file)))))
   :abspath
     (fn [this file]
       (FilenameUtils/concat (.getAbsolutePath (:root this)) file))
   :abspaths
     (fn
       ([this] (abspaths this (:fileids this)))
       ([this fileids] (abspaths this fileids false))
       ([this fileids include-encoding]
         (let [paths (map (partial abspath this) fileids)]
           (if include-encoding
              (partition 2 (interleave paths (map (partial encoding this) fileids)))
              paths))))})

(defn read-blankline-block [reader]
  ((fn [s]
     (let [line (.readLine reader)]
       (cond
         (nil? line)
           (if s [s] [])
         (string/blank? line)
           (if s [s] (recur s))
         :else
           (recur (str s line)))))
    ""))

(defn normalize-root [root]
  (if (string? root)
    (File. root)
    root))
