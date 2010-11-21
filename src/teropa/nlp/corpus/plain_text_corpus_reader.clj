(ns teropa.nlp.corpus.plain-text-corpus-reader
  (:require [clojure.contrib.duck-streams :as streams])
  (:require [teropa.nlp.tokenizer :as tok])
  (:require [teropa.nlp.tokenizer.punkt])
  (:require [teropa.nlp.tokenizer.regexp])
  (:use teropa.nlp.corpus.corpus-reader)
  (:use teropa.nlp.util))
  

(defrecord PlainTextCorpusReader [files word-tokenizer sent-tokenizer para-tokenizer encoding]
  CorpusContents
  (raw [this] (raw this files))
  (raw [this files]
    (slurp-all files))
  (words [this] (words this files))
  (words [this files]
    (mapcat
      #(tok/tokenize word-tokenizer (streams/slurp* %))
      files))
  (sents [this] (sents this files))
  (sents [this files]
    (mapcat
      (fn [path]
        (map
          #(tok/tokenize word-tokenizer %)
          (tok/tokenize sent-tokenizer (streams/slurp* path))))
      files))
  (paras [this] (paras this files))
  (paras [this files]
    (mapcat
      (fn [path]
        (map
          (fn [para]
            (map
              #(tok/tokenize word-tokenizer %)
              (tok/tokenize sent-tokenizer para)))
          (tok/tokenize para-tokenizer (streams/slurp* path))))
      files)))
  
(extend PlainTextCorpusReader CorpusReader default-reader-impls)

(defn make
  ([files]
    (make files (teropa.nlp.tokenizer.regexp/make-word-punct-tokenizer)))
  ([files word-tokenizer]
    (make files word-tokenizer (teropa.nlp.tokenizer.punkt/make-sentence-tokenizer)))
  ([files word-tokenizer sent-tokenizer]
    (make files word-tokenizer sent-tokenizer (teropa.nlp.tokenizer.regexp/make-blank-line-tokenizer)))
  ([files word-tokenizer sent-tokenizer para-tokenizer]
    (make files word-tokenizer sent-tokenizer para-tokenizer nil))
  ([files word-tokenizer sent-tokenizer para-tokenizer encoding]
      (PlainTextCorpusReader.
        files
        word-tokenizer
        sent-tokenizer
        para-tokenizer
        encoding)))

