(ns teropa.nlp.tokenizer.punkt
  "The Punkt sentence tokenizer.  The algorithm for this tokenizer is
   described in Kiss & Strunk (2006)::
     Kiss, Tibor and Strunk, Jan (2006): Unsupervised Multilingual Sentence
      Boundary Detection.  Computational Linguistics 32: 485-525."
  (:require [clojure.contrib.string :as string])
  (:use teropa.nlp.tokenizer)
  (:use teropa.nlp.tokenizer.punkt-token)
  (:use teropa.nlp.tokenizer.punkt-ortography)
  (:use teropa.nlp.util)
  (:import [java.util.regex Pattern]))
  
(def default-lang-vars
  {; Characters which are candidates for sentence boundaries
   :sent-end-chars #{"." "?" "!"} 
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
    "
   ; Format of a regular expression to find contexts including possible
   ; sentence boundaries. Matches token which the possible sentence boundary
   ; ends, and matches the following token with a lookahead expression.
   ; Placeholders are for sent-end-chars and non-word, respectively.
   :period-context-fmt "(?x)
        \\S*                 # some word material
        %1$s                 # a potential sentence ending
        (?=(
            %2$s             # either other punctuation
            |
            \\s+(\\S+)       # or whitespace and some other token
        ))"})

(defn- word-tokenizer-re
  "Compiles and returns a regular expression for word tokenization"
  [{:keys [word-tokenize-fmt re-non-word-chars re-multi-char-punct re-word-start]}]
  (Pattern/compile
    (format word-tokenize-fmt
      re-multi-char-punct
      re-word-start
      re-non-word-chars)))
          
(defn- period-context-re
  "Compiles and returns a regular expression to find contexts including
   possible sentence boundaries"
  [{:keys [period-context-fmt re-non-word-chars re-sent-end-chars]}]
  (Pattern/compile
    (format period-context-fmt
      re-sent-end-chars
      re-non-word-chars)))
  
(defn- word-tokenize
  "Tokenize a string to split of punctuation other than periods"
  [lang-vars text]
  (re-seq (word-tokenizer-re lang-vars) text))
 
(defn- realign-boundaries [sentences])

(defn- tokenize-words [tokenizer plaintext]
  "Divide the given text into tokens, using the punkt word
   segmentation regular expression, and generate a lazy seq of tokens
   augmented as with two boolean values for whether the given token occurs
   at the start of a paragraph or a new line, respectively"
  (letfn [(tokens [lines para-start]
            (lazy-seq
              (if (seq lines)
                  (let [line (first lines)]
                    (if-not (string/blank? line)
                      (let [line-toks (word-tokenize (:lang-vars tokenizer) line)]
                        (concat
                          [(make-token (first line-toks) {:parastart para-start
                                                          :linestart true})]
                          (map make-token (rest line-toks))
                          (tokens (rest lines) false)))
                      (tokens (rest lines) true)))
                  nil)))]
    (tokens (.split plaintext "\n") false)))

(defn- first-pass-annotation
  "Performs type-based annotation on a single token"
  [tokenizer token]
  (let [tok (:tok token)
        abbrev-types (:abbrev-types (:params tokenizer))]
    (cond
      (get (:sent-end-chars (:lang-vars tokenizer)) tok)
        (assoc token :sentbreak true)
      (and (:period-final token)
           (not (.endsWith tok "..")))
        (let [butlastchar (lower (substr tok 0 -1))]
          (if (or (abbrev-types butlastchar)
                  (abbrev-types (last (.split butlastchar "-"))))
            (assoc token :abbr true)
            (assoc token :sentbreak true)))
      :else token)))

(defn- second-pass-annotation
  "Performs a token-based classification over a pair of contiguous
   tokens returning an updated augmented token for the first of them"
  [tokenizer [aug-tok1 aug-tok2]]
  (cond
    (not aug-tok2) aug-tok1 ; Is it the last token? We can't do anything then.
    (not (:period-final aug-tok1)) aug-tok1 ; We only care about words ending in periods
    :else
      (let [tok (:tok aug-tok1)
            typ (type-no-period aug-tok1)
            next-tok (:tok aug-tok2)
            next-typ (type-no-sentperiod aug-tok2)
            tok-is-initial (is-initial? aug-tok1)]
        (cond
          ; [4.1.2. Collocation Heuristic] If there's a collocation
          ; between the before and after the period, then label tok as
          ; an abbreviation and NOT a sentence break. Note that collocations
          ; with frequent sentence starters as their second word are excluded
          ; in training.
          (get (:collocations (:params tokenizer)) [typ next-typ])
            (assoc aug-tok1 :sentbreak false :abbr true)
          ; [4.2 Token-Based Reclassification of Abbreviations] If
          ; the token is an abbreviation or an ellipsis, then decide
          ; we should *also* classify it as a sentbreak.
          ; [4.1.1 Ortographic Heuristic] Check if there's ortographic
          ; evidence about whether the next word starts a sentence or not
          (and (or (:abbr aug-tok1) (is-ellipsis? aug-tok1))
               (not tok-is-initial)
               (ortho-heuristic aug-tok2 (:ortho-context (:params tokenizer))))
            (assoc aug-tok1 :sentbreak true)
          ; [4.1.3 Frequent Sentence Starter Heuristic] If the next word
          ; is capitalized, and is a member of the frequent-sentence-starters
          ; list, then label tok as a sentence break.
          (and (or (:abbr aug-tok1) (is-ellipsis? aug-tok1))
               (not (is-initial? aug-tok1))
               (first-upper? aug-tok2)
               (get (:sent-starters (:params tokenizer)) next-typ))         
            (assoc aug-tok1 :sentbreak true)
          ; [4.3 Token-Based Detection of Initials and Ordinals] 
          ; Check if any initials or ordinals tokens that are marked
          ; as sentbreaks should be reclassified as abbreviations
          (or tok-is-initial (= typ "##number##"))
            ; [4.1.1 Ortographic Heuristic] Check if there's 
            ; ortographic evidence about whether the next word 
            ; stats a sentence or not.
            (let [is-sent-starter (ortho-heuristic aug-tok2)]
              (if-not is-sent-starter
                (assoc aug-tok1 :sentbreak false, :abbr true)
                ; Special heuristic for initials: if ortographic heuristic
                ; is unknown, and next word is always capitalized, then mark
                ; as abbrev (eg: J. Bach)
                (if (and (= is-sent-starter :unknown)
                         tok-is-initial
                         (first-upper? aug-tok2)
                         (not (bit-and (get (:ortho-context (:params tokenizer)) next-typ)
                                       ortho-lc)))
                  (assoc aug-tok1 :sentbreak false, :abbr true)
                  aug-tok1)))
          :else aug-tok1))))

          
    
  
(defn- annotate-first-pass
  "Perform the first pass of annotation, which makes decisions 
   based purely on the word type of each word.
   - '?', '!', and '.' are marked as sentence breaks.
   - sequences of two or more periods are marked as ellipsis
   - any word ending in '.' that's a known abbreviation is marked
     as an abbreviation
   - any other word ending in '.' is marked as a sentence break"
  [tokens tokenizer]
  (map (partial first-pass-annotation tokenizer) tokens))

(defn- annotate-second-pass
  "Performs a token-base classification (section 4) over the given
   tokens, making use of the ortographic heuristic (4.1.1), collocation
   heuristic (4.1.2) and frequent sentence starter heuristic (4.1.3)"
  [tokens tokenizer]
  (map (partial second-pass-annotation tokenizer) (pairs tokens)))

(defn- annotate-tokens
  "Given a seq of tokens augmented with markers for line-start and
   paragraph-start, returns a lazy seq of those tokens with full
   annotation including predicted sentence breaks."
  [tokens tokenizer]
  (-> tokens
      ; Make a preliminary first pass through the document, marking likely
      ; sentence breaks, abbreviations, and ellipsis tokens.
      (annotate-first-pass tokenizer)
      ; Make a second pass through the document, using token context information
      ; to change our preliminary decisions about where sentence breaks,
      ; abbreviations, and ellipses occur.
      (annotate-second-pass tokenizer)))

(defn- text-contains-sentbreak?
  "Return true if the given text includes a sentence break"
  [text tokenizer]
  (some :sentbreak
    (butlast (annotate-tokens (tokenize-words tokenizer text) tokenizer))))

(defn- sentences-from-text
  "Given a text, generates the sentences in that text by only testing
   candidate sentence breaks. If realign-boundaries? is true, includes
   in the sentence closing punctuation that follows the period"
  ([tokenizer text realign-boundaries?]
    (let [sents (sentences-from-text tokenizer text)]
      (if realign-boundaries? (realign-boundaries sents) sents)))
  ([tokenizer text]
    (letfn [(last-break-for-next [matcher]
              (if (.group matcher 2) 
                (.start matcher 2) ; Next sentence starts after whitespace
                (.end matcher))) ; Next sentence starts at following punctuation
            (next-match [matcher last-break]
              (lazy-seq
                (if (.find matcher)
                    (if (text-contains-sentbreak?
                          (str (.group matcher) (.group matcher 1))
                          tokenizer)
                      (cons
                        (.substring text last-break (.end matcher))
                        (next-match
                          matcher
                          (last-break-for-next matcher)))
                      (next-match matcher last-break))
                  [(.substring text last-break)])))]
      (next-match
        (.matcher (period-context-re (:lang-vars tokenizer)) text)
        0))))
    

(deftokenizer PunktWordTokenizer [lang-vars]
  (tokenize [this text] (tokenize this text nil))
  (tokenize [this text args]
    (word-tokenize lang-vars text)))

(defn make-word-tokenizer
  ([] (make-word-tokenizer default-lang-vars))
  ([lang-vars] (PunktWordTokenizer. lang-vars)))

(deftokenizer PunktSentenceTokenizer [params lang-vars]
  (tokenize [this text] (tokenize this text {}))
  (tokenize [this text {:keys [realign-boundaries]}]
    (sentences-from-text this text realign-boundaries)))

(defn make-sentence-tokenizer
  ([] (make-sentence-tokenizer (slurp-form* "resources/tokenizers/punkt/english.clj")))
  ([params] (make-sentence-tokenizer params default-lang-vars))
  ([params lang-vars] (PunktSentenceTokenizer. params lang-vars)))
  