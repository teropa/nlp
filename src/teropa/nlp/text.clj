(ns teropa.nlp.text
  (:require [teropa.nlp.concordance :as concordance]))

(defrecord Text [tokens])

(defn make-text [text]
  (Text. text))

