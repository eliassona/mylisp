(ns mylisp.parser-test
  (:require [clojure.test :refer :all]
            [mylisp.parser :refer :all]))

(deftest a-test
  (is (= [1] (expr-of "[[1]]" 1)))
  (is (= [[1]] (expr-of "[[1]]" 0)))
  (is (= [1 [1] [[1]]] (exprs-of "[[1]]")))
  (let [e (exprs-of "[[1]")]  
    (is (= [1 [1]] e))
    (is (-> e meta :failure)))
  )

(deftest test-parent-expr
  (let [text "[:root0 [:sub0 [:subsub0] :sub1] :root1]"
        index-pairs (index-pairs-of text)]
    (is (= :subsub0 (index-pair->expr text (index-pair-of index-pairs 19))))
    #_(is (= "[:subsub0]" (parent-index-of index-pairs 19)))
    )
  )