(ns mylisp.eval_test
  (:require [mylisp.eval :refer [fn-app new-eval]]
            [clojure.test :refer [is deftest run-tests ]]))




(deftest verify-fn-app
  (let [fn-def '(([x] x) ([x y] (if (> x y) x y)) ([x y & more] 3))]
    (is (= 1 (fn-app fn-def '(1) {})))
    (is (= 2 (fn-app fn-def '(0 2) {})))
;   (is (= 4 (fn-app fn-def '(0 2 4) {})))
;    (is (= 4 (fn-app fn-def '(0 2 4 -1) {})))
  ))

(deftest verify-eval 
  (new-eval '(def a (quote (1 2 3))) {})
  (is (= [2 3] (new-eval '(if (empty? a) (first a) (rest a)) {})))
  (is (= "empty" (new-eval '(if (empty? (quote ())) "empty" "full") {})))
  (is (= 1 (new-eval '(+ (first (list 1 2 3))) {})))
 (is (= 6 (new-eval '(reduce + 0 (list 1 2 3)) {})))
  (is (= (list 2 3) (new-eval '((fn [coll] (rest coll)) (list 1 2 3)) {})))
  )
