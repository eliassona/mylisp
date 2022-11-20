(ns mylisp.parser-test
  (:require [clojure.test :refer :all]
            [mylisp.parser :refer :all]))

(deftest a-test
  (is (= "[1]" (expr-of "[[1]]" 1)))
  (is (= "[[1]]" (expr-of "[[1]]" 0)))
  (is (= ["1" "[1]" "[[1]]"] (exprs-of "[[1]]")))
  (let [e (exprs-of "[[1]")]  
    (is (= ["1" "[1]"] e))
    (is (-> e meta :failure)))
  )

(deftest test-parent-expr
  (let [
  )