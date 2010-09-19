(ns teropa.nlp.tokenizer)

(defprotocol Tokenizer
  (tokenize [this s])
  (batch-tokenize [this s]))

