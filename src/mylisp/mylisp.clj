(ns mylisp.mylisp
  (:require [mylisp.parser :refer [parser ast->clj]]
            [mylisp.eval :refer [eval-expr]]))

(defn evaluate [text]
  (eval-expr (-> text parser ast->clj) {} false))