(ns teropa.nlp.util-tests
  (:use clojure.test)
  (:use teropa.nlp.util))

(deftest string-convenience-funcs
  
  (is (= "    " (blank-string 4)))
  (is (= ""     (blank-string 0)))
  
  (is (= "ello" (substr "hello" 1)))
  (is (= "hell" (substr "hello" 0  4)))
  (is (= "lo"   (substr "hello" -2)))
  (is (= "ell"  (substr "hello" 1  -1)))
  (is (= "hell" (substr "hello" 0 -1)))
  
  (is (= "have a nice day" (join-words ["have" "a" "nice" "day"]))))


(deftest pattern-nongrouping-conversion
  
  (is (= "a" (convert-regexp-to-nongrouping "a")))
  (is (= "a.b[0-9]" (convert-regexp-to-nongrouping "a.b[0-9]")))
  (is (= "(?:a)" (convert-regexp-to-nongrouping "(a)")))
  (is (= "(?:a)" (convert-regexp-to-nongrouping "(?:a)")))
  (is (= "\\(" (convert-regexp-to-nongrouping "\\(")))
  (is (= "(?:\\()" (convert-regexp-to-nongrouping "(\\()")))
  (is (= "\\((?:.*)\\)" (convert-regexp-to-nongrouping "\\((.*)\\)")))
  (is (= "\\s*\\n\\s*\\n\\s*" (convert-regexp-to-nongrouping #"\s*\n\s*\n\s*")))
  (is (= "\\w+|[^\\w\\s]+" (convert-regexp-to-nongrouping #"\w+|[^\w\s]+"))))









  
  (run-tests)

