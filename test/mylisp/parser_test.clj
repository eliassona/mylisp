(ns mylisp.parser-test
  (:require [clojure.test :refer :all]
            [mylisp.parser :refer :all]))

(deftest a-test
  (is (= ["[1]" [1 4]] (expr-of (exprs-of "[[1]]") 1)))
  (is (= ["[[1]]" [0 5]] (expr-of (exprs-of "[[1]]") 0)))
  (is (= #{["[[1]]" [0 5]] ["1" [2 3]] ["[1]" [1 4]]} (exprs-of "[[1]]")))
  (let [e (exprs-of "[[1]")]  
    (is (= #{["1" [2 3]] ["[1]" [1 4]]} e))
    (is (-> e meta :failure)))
  )

(deftest test-parent-expr
  (let [text "[:root0 [:sub0 [:subsub0] :sub1] :root1]"
        exprs (exprs-of text)]
    (is (= ":subsub0" (first (expr-of exprs 17))))
    (let [exprs (disj exprs (expr-of exprs 17))]
      (is (= "[:subsub0]" (first (expr-of exprs 17))))
      (let [exprs (disj exprs (expr-of exprs 17))]
        (is (= "[:sub0 [:subsub0] :sub1]" (first (expr-of exprs 17))))
        (let [exprs (disj exprs (expr-of exprs 17))]
          (is (= text (first (expr-of exprs 17))))
          )
        )
      )
    
  
  ))