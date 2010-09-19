(ns teropa.nlp.tokenizer.punkt
  "The Punkt sentence tokenizer.  The algorithm for this tokenizer is
   described in Kiss & Strunk (2006)::

     Kiss, Tibor and Strunk, Jan (2006): Unsupervised Multilingual Sentence
      Boundary Detection.  Computational Linguistics 32: 485-525."
  (:use teropa.nlp.tokenizer)
  (:import [java.util.regex Pattern]))
  
(def default-lang-vars
  {; Characters which are candidates for sentence boundaries
   :sent-end-chars #{\. \? \!} 
   :re-sent-end-chars "[.?!]"
   ; Sentence internal punctuation, which indicates an abbreviation if preceeded by a period-final token
   :internal-punctuation ",:;" 
   ; Used to realign punctuation that should be included in a sentence although it follows the period (or ?, !)
   :re-boundary-realignment #"(?m)[\"\')\]}]+?(?:\s+|(?=--)|$)"
   ; Excludes some characters from starting word tokens
   :re-word-start "[^\\(\\\"\\`{\\[:;&\\#\\*@\\)}\\]\\-,]"
   ; Characters that cannot appear within words
   :re-non-word-chars "(?:[?!)\\\";}\\]\\*:@'\\({\\[])"
   ; Hyphen and ellipsis are multi-character punctuation
   :re-multi-char-punct "(?:\\-{2,}|\\.{2,}|(?:\\.\\s){2,}\\.)"
   ; Format of a regular expression to split punctuation from words, excluding period.
   ; Placeholders are for multichar, wordstart, and nonword, respectively
   :word-tokenize-fmt "(?x)
        %1$s
        |
        (?=%2$s)\\S+?   # Accept word characters until end is found
        (?= # Sequences marking a word's end
            \\s|                                 # White-space
            $|                                  # End-of-string
            %3$s|%1$s|          # Punctuation
            ,(?=$|\\s|%3$s|%1$s) # Comma if at end of word
        )
        |
        \\S
    "})
   
(defn- word-tokenizer-re
  "Compiles and returns a regular expression for word tokenization"
  [{:keys [word-tokenize-fmt re-non-word-chars re-multi-char-punct re-word-start]}]
  (Pattern/compile
    (format word-tokenize-fmt
      re-multi-char-punct
      re-word-start
      re-non-word-chars)))
          
  
(defn- word-tokenize
  "Tokenize a string to split of punctuation other than periods"
  [lang-vars text]
  (re-seq (word-tokenizer-re lang-vars) text))
  
(deftokenizer PunktWordTokenizer [lang-vars]
  (tokenize [this text]
    (word-tokenize lang-vars text)))

