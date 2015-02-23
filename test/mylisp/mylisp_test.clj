(ns mylisp.mylisp_test
  (:require [mylisp.mylisp :refer [evaluate]]
            [clojure.test :refer [run-tests is deftest]])
            )



(deftest verify-self-eval
  (is (= 1 (evaluate "1")))
  (is (= "a str" (evaluate "\"a str\"")))
  (is (= :keyword (evaluate ":keyword")))
  (is (= 1.23 (evaluate "1.23")))
  (is (= false (evaluate "false")))
;  (is (= true (evaluate "true")))
  ;(is (= nil (evaluate "nil")))
  
  (is (= 1 (evaluate "'1")))
  (is (= 1 (evaluate "`1")))
  (is (= 1 (evaluate "`~1")))
  )