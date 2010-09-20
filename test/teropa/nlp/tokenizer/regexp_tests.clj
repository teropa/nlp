(ns teropa.nlp.tokenizer.simple-tests
  (:use clojure.test)
  (:use teropa.nlp.tokenizer)
  (:use teropa.nlp.tokenizer.regexp))


(deftest blank-line-tokenizer-tests
  (let [tokenizer (teropa.nlp.tokenizer.regexp.BlanklineTokenizer.)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello\n\nworld" ["hello" "world"]
      "hello\n \n!\n\n \nworld" ["hello" "!" "world"])))

(deftest word-punct-tokenizer-tests
  (let [tokenizer (teropa.nlp.tokenizer.regexp.WordPunctTokenizer.)]
    (are [input output] (= (tokenize tokenizer input) output)
      "hello" ["hello"]
      "hello world" ["hello" "world"]
      "She said 'hello'." ["She" "said" "'" "hello" "'."])))


(run-tests)
