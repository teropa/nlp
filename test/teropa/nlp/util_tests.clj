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

(deftest number-convenience-funcs
  
  (is (= 0.33 (round (/ 1 3) 2))))


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

(deftest ingram-creation
  (is (= [[1 2] [2 3] [3 4]] (ingrams [1 2 3 4] 2)))
  (is (= [["w0t" 1] [1 2] [2 3] [3 4]] (ingrams [1 2 3 4] 2 true false "w0t")))
  (is (= [["w0t" 1] [1 2] [2 3] [3 4] [4 "w0t"]] (ingrams [1 2 3 4] 2 true true "w0t")))
  (is (= [["w0t" "w0t"]] (ingrams [] 2 true true "w0t")))
  (is (= [] (ingrams [] 2))))



(run-tests)

