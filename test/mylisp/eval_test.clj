(ns mylisp.eval_test
  (:require [mylisp.eval :refer [fn-app evl]]
            [clojure.test :refer [is deftest run-tests ]]))




(deftest verify-fn-app
  (let [fn-def '(([x] x) ([x y] (if (> x y) x y)) ([x y & more] 3))]
    (is (= 1 (fn-app fn-def '(1) {})))
    (is (= 2 (fn-app fn-def '(0 2) {})))
;   (is (= 4 (fn-app fn-def '(0 2 4) {})))
;    (is (= 4 (fn-app fn-def '(0 2 4 -1) {})))
  ))

(deftest verify-eval 
  (evl (def a (quote (1 2 3))))
  (is (= [2 3] (evl (if (empty? a) (first a) (rest a)))))
  (is (= "empty" (evl (if (empty? (quote ())) "empty" "full"))))
  (is (= 1 (evl (+ (first (list 1 2 3))))))
  (is (= 6 (evl (reduce + 0 (list 1 2 3)))))
  (is (= (list 2 3) (evl ((fn [coll] (rest coll)) (list 1 2 3)))))
  )
