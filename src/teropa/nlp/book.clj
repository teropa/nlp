(ns teropa.nlp.book
  (:use [teropa.nlp.corpus.corpus-reader])
  (:require [teropa.nlp.corpus.plain-text-corpus-reader :as plaintext]))

(def gutenberg (plaintext/make "resources/corpora/gutenberg" #"[^?!\.].*\.txt"))
(def genesis (plaintext/make "resources/corpora/genesis" #"[^?!\.].*\.txt"))
(def inaugural (plaintext/make "resources/corpora/inaugural" #"[^?!\.].*\.txt"))

(def text1 (words gutenberg ["melville-moby_dick.txt"]))
(def text2 (words gutenberg ["austen-sense.txt"]))
(def text3 (words genesis ["english-kjv.txt"]))
(def text4 (words inaugural))
(def text5 (words gutenberg ["chesterton-thursday.txt"]))
