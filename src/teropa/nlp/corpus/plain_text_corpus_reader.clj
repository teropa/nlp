(ns teropa.nlp.corpus.plain-text-corpus-reader
  (require [teropa.nlp.corpus.corpus-reader :as api]))

(defrecord PlainTextCorpusReader [root fileids word-tokenizer sent-tokenizer para-block-reader encoding])
(extend PlainTextCorpusReader api/CorpusReader api/default-impls)

(defn make
  ([root fileids]
    (make root fileids (teropa.nlp.tokenizer.punkt/make-word-tokenizer)))
  ([root fileids word-tokenizer]
    (make root fileids word-tokenizer (teropa.nlp.tokenizer.punkt/make-sentence-tokenizer)))
  ([root fileids word-tokenizer sent-tokenizer]
    (make root fileids word-tokenizer sent-tokenizer api/read-blankline-block))
  ([root fileids word-tokenizer sent-tokenizer para-block-reader]
    (make root fileids word-tokenizer sent-tokenizer para-block-reader nil))
  ([root fileids word-tokenizer sent-tokenizer para-block-reader encoding]
    (PlainTextCorpusReader. root fileids word-tokenizer sent-tokenizer para-block-reader encoding)))

