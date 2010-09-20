(ns teropa.nlp.tokenizer)

(defprotocol Tokenizer
  (tokenize [this s] [this s args])
  (batch-tokenize [this s] [this s args]))

(defmacro deftokenizer [name init-vec & body]
  (let [method-names (into #{} (map first (filter list? body)))
        body-with-default-impls
          (concat body
            (if-not (method-names 'tokenize)
              [`(tokenize [this# s#] (tokenize this# s# nil))
               `(tokenize [this# s# args#]
                  (throw (UnsupportedOperationException.
                           "tokenize not supported by this tokenizer")))]
            (if-not (method-names 'batch-tokenize)
              [`(batch-tokenize [this# s#] (batch-tokenize this# s# nil))
               `(batch-tokenize [this# s# args#]
                  (throw (UnsupportedOperationException.
                           "batch-tokenize not supported by this tokenizer")))])))]
    
    `(defrecord ~name ~init-vec
       teropa.nlp.tokenizer/Tokenizer
       ~@(remove nil? body-with-default-impls))))
