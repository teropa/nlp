(ns teropa.nlp.corpus.plain-text-corpus-reader
  (:require [clojure.contrib.duck-streams :as streams])
  (:require [teropa.nlp.tokenizer :as tok])
  (:use teropa.nlp.corpus.corpus-reader)
  (:use teropa.nlp.util))
  

(defrecord PlainTextCorpusReader [root fileids word-tokenizer sent-tokenizer para-block-tokenizer encoding])
(extend PlainTextCorpusReader CorpusReader
  (merge default-impls
    {:raw
       (fn
         ([this] (raw this (:fileids this)))
         ([this fileids]
            (slurp-all (abspaths this fileids))))
     :words
       (fn
         ([this] (words this (:fileids this)))
         ([this fileids]
           (mapcat
             #(tok/tokenize (:word-tokenizer this) (streams/slurp* %))
             (abspaths this fileids))))
     :sents
       (fn 
         ([this] (sents this (:fileids this)))
         ([this fileids]
           (mapcat
             (fn [path]
               (map
                 #(tok/tokenize (:word-tokenizer this) %)
                 (tok/tokenize (:sent-tokenizer this) (streams/slurp* path))))
             (abspaths this fileids))))
     :paras
       (fn
         ([this] (paras this (:fileids this)))
         ([this fileids]
           (mapcat
             (fn [path]
               (map
                 (fn [para]
                   (map
                     #(tok/tokenize (:word-tokenizer this) %)
                     (tok/tokenize (:sent-tokenizer this) para)))
                 (tok/tokenize (:para-block-tokenizer this) (streams/slurp* path))))
             (abspaths this fileids))))}))
         
(defn make
  ([root fileids]
    (make root fileids (teropa.nlp.tokenizer.regexp/make-word-punct-tokenizer)))
  ([root fileids word-tokenizer]
    (make root fileids word-tokenizer (teropa.nlp.tokenizer.punkt/make-sentence-tokenizer)))
  ([root fileids word-tokenizer sent-tokenizer]
    (make root fileids word-tokenizer sent-tokenizer (teropa.nlp.tokenizer.regexp/make-blank-line-tokenizer)))
  ([root fileids word-tokenizer sent-tokenizer para-block-reader]
    (make root fileids word-tokenizer sent-tokenizer para-block-reader nil))
  ([root fileids word-tokenizer sent-tokenizer para-block-reader encoding]
    (let [root (normalize-root root)]
      (PlainTextCorpusReader.
        root
        (filenames-by-regex root fileids)
        word-tokenizer
        sent-tokenizer
        para-block-reader
        encoding))))

