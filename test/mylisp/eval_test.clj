(ns mylisp.eval_test
  (:require [mylisp.eval :refer [fn-app evl]]
            [clojure.test :refer [is deftest run-tests ]]))




(deftest verify-fn-app
  (let [fn-def '(([x] x) ([x y] (if (> x y) x y)) ([x y & more] (reduce max (max x y) more)))]
    (is (= 1 (fn-app fn-def '(1) {} false)))
    (is (= 2 (fn-app fn-def '(0 2) {} false)))
    (is (= 4 (fn-app fn-def '(0 2 4) {} false)))
    (is (= 4 (fn-app fn-def '(0 2 4 -1) {} false)))
  ))

(deftest verify-eval 
  (evl (def a (quote (1 2 3))))
  (is (= [2 3] (evl (if (empty? a) (first a) (rest a)))))
  (is (= "empty" (evl (if (empty? (quote ())) "empty" "full"))))
  (is (= 1 (evl (+ (first (list 1 2 3))))))
  (is (= 6 (evl (reduce + 0 (list 1 2 3)))))
  (is (= [2 3] (evl ((fn [coll] (rest coll)) (list 1 2 3)))))
  (is (= [2 3] (evl ((fn [coll] (rest coll)) (quote (1 2 3))))))
  (is (= [1 2 3] (evl ((fn [coll] (rest coll)) (quote (list 1 2 3))))))
  (is (= 3 (evl (count (list 1 2 (list 3 4))))))
  (is (= [2 [3 4]] (evl (rest (list 1 2 (list 3 4))))))
  (is (= [[3 4]] (evl (rest (rest (list 1 2 (list 3 4)))))))
  (is (= [3 4] (evl (first (rest (rest (list 1 2 (list 3 4))))))))
  (is (= 'asdf (evl (syntax-quote asdf))))
  (is (= '(+ 1 2) (evl (syntax-quote (+ 1 2)))))
  (is (= 3 (evl (syntax-quote (unquote (+ 1 2))))))
  (is (= '(3) (evl (syntax-quote ((unquote (+ 1 2)))))))
  (is (= '(3 (1 2 a)) (evl (syntax-quote ((unquote (+ 1 2)) (1 2 a))))))
  (is (= '(if (> 3 3) 1 0) (evl (syntax-quote (if (> (unquote (+ 1 2) ) 3) 1 0)))))
  (is (= '(def b 6) (evl (syntax-quote (def b (unquote (+ 1 2 3)))))))
 ; (is (= '(fn [x] (+ x 6)) (evl (syntax-quote (fn [x] (+ x (unquote (+ 1 2 3))))))))
  )
