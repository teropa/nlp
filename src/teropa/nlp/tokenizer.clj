(ns teropa.nlp.tokenizer)

(defprotocol Tokenizer
  (tokenize [this s])
  (batch-tokenize [this s]))

(defmacro deftokenizer [name init-vec & body]
  (let [method-names (apply hash-set (map first (filter list? body)))
        body (conj body
               (if-not (method-names 'tokenize)
                 `(tokenize [this# s#]
                    (throw (UnsupportedOperationException.
                             "tokenize not supported by this tokenizer"))))
               (if-not (method-names 'batch-tokenize)
                 `(batch-tokenize [this# s#]
                    (throw (UnsupportedOperationException.
                             "batch-tokenize not supported by this tokenizer")))))]
    
    `(deftype ~name ~init-vec
       teropa.nlp.tokenizer/Tokenizer
       ~@(remove nil? body))))
