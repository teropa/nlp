(ns teropa.nlp.corpus
  (:use [teropa.nlp.corpus.corpus-reader])
  (:require [teropa.nlp.corpus.plain-text-corpus-reader :as plaintext]))

(def gutenberg (plaintext/make "resources/corpora/gutenberg" #"[^?!\.].*\.txt"))
