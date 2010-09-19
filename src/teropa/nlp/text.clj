(ns teropa.nlp.text)

(defrecord Text [tokens])

(extend Text
  teropa.nlp.concordance/Concordance
    teropa.nlp.concordance/text-impl)
