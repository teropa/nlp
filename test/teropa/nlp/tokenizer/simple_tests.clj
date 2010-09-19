(ns teropa.nlp.tokenizer.simple-tests
  (:use clojure.test)
  (:use teropa.nlp.tokenizer)
  (:use teropa.nlp.tokenizer.simple))


(deftest whitespace-tokenizer-tests
  (let [tokenizer (teropa.nlp.tokenizer.simple.WhitespaceTokenizer.)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello world" ["hello" "world"]
      "a\nb\tc de"  ["a" "b" "c" "de"])))

(deftest space-tokenizer-tests
  (let [tokenizer (teropa.nlp.tokenizer.simple.SpaceTokenizer.)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello world" ["hello" "world"]
      "a\nb\tc de"  ["a\nb\tc" "de"])))

(deftest tab-tokenizer-tests
  (let [tokenizer (teropa.nlp.tokenizer.simple.TabTokenizer.)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello\tworld" ["hello" "world"]
      "a\nb\tc de"  ["a\nb" "c de"])))

(deftest char-tokenizer-tests
  (let [tokenizer (teropa.nlp.tokenizer.simple.CharTokenizer.)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello\tworld" ["h" "e" "l" "l" "o" "\t" "w" "o" "r" "l" "d"]
      "a\nb\tc de"  ["a" "\n" "b" "\t" "c" " " "d" "e"])))

(deftest line-tokenizer-tests
  (let [tokenizer (teropa.nlp.tokenizer.simple.LineTokenizer. :keep)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello\n \nworld\n " ["hello" " " "world" " "]
      "a\nb\tc de"  ["a" "b\tc de"]))
  (let [tokenizer (teropa.nlp.tokenizer.simple.LineTokenizer. :discard)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello\n \nworld\n " ["hello" "world"]))
  (let [tokenizer (teropa.nlp.tokenizer.simple.LineTokenizer. :discard-eof)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello\n \nworld\n " ["hello" " " "world"])))
(run-tests)
