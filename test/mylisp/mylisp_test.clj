(ns mylisp.mylisp_test
  (:require [mylisp.mylisp :refer [evaluate]]
            [clojure.test :refer [run-tests is deftest]])
            )



(deftest verify-self
  (is (= 1 (evaluate "1")))
  (is (= "a str" (evaluate "\"a str\"")))
  (is (= :keyword (evaluate ":keyword")))
  (is (= 1.23 (evaluate "1.23")))
  (is (= nil (evaluate "false")))
  (is (= true (evaluate "true")))
  (is (= nil (evaluate "nil")))
  (is (= 1 (evaluate "'1")))
  (is (= 1 (evaluate "`1")))
  (is (= 1 (evaluate "`~1")))
  )

(deftest verify-def
  (is (= nil (evaluate "(def xyz 10)")))
  (is (= 10 (evaluate "xyz")))
  (is (= nil (evaluate "(def xyz 11)")))
  (is (= 11 (evaluate "xyz")))
  (is (= nil (evaluate "(def a-fn (fn [x] (+ x 1)))")))
  (is (= 2 (evaluate "(a-fn 1)")))
  (is (= '([x] (+ x 1)) (evaluate "a-fn")))
  )

(deftest verify-if 
  (is (= "2" (evaluate "(if (= 1 2) \"1\" \"2\")")))
  (is (= "1" (evaluate "(if (= 1 1) \"1\" \"2\")")))
  )

(deftest verify-do
  (is (= 3 (evaluate "(do 1 2 (+ 1 2))")))
  )

