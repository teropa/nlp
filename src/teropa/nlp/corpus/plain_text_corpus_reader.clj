(ns teropa.nlp.corpus.plain-text-corpus-reader
  (:require [clojure.contrib.duck-streams :as streams])
  (:require [teropa.nlp.tokenizer :as tok])
  (:require [teropa.nlp.tokenizer.punkt])
  (:require [teropa.nlp.tokenizer.regexp])
  (:use teropa.nlp.corpus.corpus-reader)
  (:use teropa.nlp.util))
  

(defrecord PlainTextCorpusReader [root fileids word-tokenizer sent-tokenizer para-tokenizer encoding]
  CorpusContents
  (raw [this] (raw this fileids))
  (raw [this fileids]
    (slurp-all (abspaths this fileids)))
  (words [this] (words this fileids))
  (words [this fileids]
    (mapcat
      #(tok/tokenize word-tokenizer (streams/slurp* %))
      (abspaths this fileids)))
  (sents [this] (sents this fileids))
  (sents [this fileids]
    (mapcat
      (fn [path]
        (map
          #(tok/tokenize word-tokenizer %)
          (tok/tokenize sent-tokenizer (streams/slurp* path))))
      (abspaths this fileids)))
  (paras [this] (paras this fileids))
  (paras [this fileids]
    (mapcat
      (fn [path]
        (map
          (fn [para]
            (map
              #(tok/tokenize word-tokenizer %)
              (tok/tokenize sent-tokenizer para)))
          (tok/tokenize para-tokenizer (streams/slurp* path))))
      (abspaths this fileids))))
  
(extend PlainTextCorpusReader CorpusReader default-reader-impls)

(defn make
  ([root fileids]
    (make root fileids (teropa.nlp.tokenizer.regexp/make-word-punct-tokenizer)))
  ([root fileids word-tokenizer]
    (make root fileids word-tokenizer (teropa.nlp.tokenizer.punkt/make-sentence-tokenizer)))
  ([root fileids word-tokenizer sent-tokenizer]
    (make root fileids word-tokenizer sent-tokenizer (teropa.nlp.tokenizer.regexp/make-blank-line-tokenizer)))
  ([root fileids word-tokenizer sent-tokenizer para-tokenizer]
    (make root fileids word-tokenizer sent-tokenizer para-tokenizer nil))
  ([root fileids word-tokenizer sent-tokenizer para-tokenizer encoding]
    (let [root (normalize-root root)]
      (PlainTextCorpusReader.
        root
        (filenames-by-regex root fileids)
        word-tokenizer
        sent-tokenizer
        para-tokenizer
        encoding))))

