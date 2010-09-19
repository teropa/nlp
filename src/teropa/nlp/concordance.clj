(ns teropa.nlp.concordance)

(defprotocol Concordance
  (concordance [text word] [text word width] [text word width lines]))

(def text-impl
  {:concordance
   (fn
     ([text word]
       (concordance text word 75))
     ([text word width]
       (concordance text word width 25))
     ([text word width lines]))})
  
   