(ns mylisp.mylisp_test
  (:require [mylisp.mylisp :refer [evaluate]]
            [clojure.test :refer [run-tests is deftest]])
            )

(deftest verify-self
  (is (= 1 (evaluate "1")))
  (is (= 1 (evaluate "0x1")))
  (is (= 255 (evaluate "0xff")))
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

(deftest verify-vector 
  (is (= [] (evaluate "[]")))
  (is (= [1] (evaluate "[1]")))
  (is (= [1 "a str" :abc 3 [2]] (evaluate "[1 \"a str\" :abc (+ 1 2) [(- 4 2)]]")))
  )

(deftest verify-lambda
  (is (= '([x] (+ x 1)) (evaluate "(fn [x] (+ x 1))")))
  (is (= 11 (evaluate "((fn [x] (+ x 1)) 10)")))
  )

(deftest verify-quote 
  (is (= 1 (evaluate "'1")))
  (is (= '(1) (evaluate "'(1)")))
  (is (= '(1 (+ 1 2)) (evaluate "'(1 (+ 1 2))")))
  )

(deftest verify-syntax-quote
  (is (= '(1 (+ 1 2)) (evaluate "`(1 (+ 1 2))")))
  (is (= '(1 3) (evaluate "`(1 ~(+ 1 2))")))
  (is (= '(1 5) (evaluate "`(1 ~(+ 1 2 `~(max 1 2)))")))
)

(deftest verify-arity
  (is (= 1 (evaluate "((fn ([x] x) ([x y] (+ x y)) ([x y z] (+ x y z))) 1)")))
  (is (= 2 (evaluate "((fn ([x] x) ([x y] (+ x y)) ([x y z] (+ x y z))) 1 1)")))
  (is (= 3 (evaluate "((fn ([x] x) ([x y] (+ x y)) ([x y z] (+ x y z))) 1 1 1)")))
  )

(deftest verify-vararg
  (is (= 1 (evaluate "((fn ([x] x) ([x y] (+ x y)) ([x y & more] (+ x y (reduce + 0 more)))) 1)")))
  (is (= 2 (evaluate "((fn ([x] x) ([x y] (+ x y)) ([x y & more] (+ x y (reduce + 0 more)))) 1 1)")))
  (is (= 3 (evaluate "((fn ([x] x) ([x y] (+ x y)) ([x y & more] (+ x y (reduce + 0 more)))) 1 1 1)")))
  (is (= 4 (evaluate "((fn ([x] x) ([x y] (+ x y)) ([x y & more] (+ x y (reduce + 0 more)))) 1 1 1 1)")))
  )

(deftest verify-macros
  (evaluate "(def xyz 99)")
  (evaluate "(def dbg1 (fn [x] `(let [x1 ~x] ['~x x1])) :macro)")
  (is (= ['xyz 99] (evaluate "(dbg1 xyz)")))
  )
