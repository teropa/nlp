(ns teropa.nlp.tokenizer.punkt-ortography
  (:use teropa.nlp.tokenizer.punkt-token)
  (:use teropa.nlp.util))

(def punctuation #(";" ":" "," "." "!" "?"))

(def ortho-beg-uc (bit-shift-left 1 1)) ; Beginning of a sentence with upper case
(def ortho-mid-uc (bit-shift-left 1 2)) ; Middle of a sentence with upper case
(def ortho-unk-uc (bit-shift-left 1 3)) ; Unknown position in a sentence with upper case
(def ortho-beg-lc (bit-shift-left 1 4)) ; Beginning of a sentence with lower case
(def ortho-mid-lc (bit-shift-left 1 5)) ; Middle of a sentence with lower case
(def ortho-unk-lc (bit-shift-left 1 6)) ; Unknown position in a sentence with lower case
(def ortho-uc (+ ortho-beg-uc ortho-mid-uc ortho-unk-uc)) ; Occurs with upper case
(def ortho-lc (+ ortho-beg-lc ortho-mid-lc ortho-unk-lc)) ; Occurs with lower case

; A map from context position and first-letter case to the appropriate
; ortographic context flag
(def ortho-map
  {[:initial :upper] ortho-beg-uc
   [:internal :upper] ortho-mid-uc
   [:unknown :upper] ortho-unk-uc
   [:initial :lower] ortho-beg-lc
   [:internal :lower] ortho-mid-lc
   [:unknown :lower] ortho-unk-lc})


(defn ortho-heuristic
  "Decide whether the given token is the first token in a sentence"
  [token context]
  (let [ctx (get context (type-no-sentperiod token) 0)]
    (cond
      ; Sentences don't start with punctuation marks
      (punctuation (:tok token))
	      false
      ; If the word is capitalized, occurs at least once with a lower case
      ; first letter, and never occurs with an upper case first letter
      ; sentence-internally, then it's a sentence starter.
      (and (first-upper? token)
           (nonzero? (bit-and ctx ortho-lc))
	         (zero? (bit-and ctx ortho-mid-uc)))
	      true
      ; If the word is lower case, and either (a) we've seen it used
      ; with upper case, or (b) we've never seen it used sentence-initially
      ; with lower case, then it's not a sentence starter.
	    (and (first-lower? token)
	         (or (nonzero? (bit-and ctx ortho-uc))
	             (zero? (bit-and ctx ortho-beg-lc))))
	      false
      ; Otherwise we're not sure
	    :else :unknown)))

