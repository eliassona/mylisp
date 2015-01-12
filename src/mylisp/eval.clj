(ns mylisp.eval
  (:require [clojure.repl :refer [source doc]]))

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(def vars (atom {}))

(defprotocol MyEval
  (my-eval [expr env]))

(defn multiple-arity? [f]
  (-> f first list?))

(defn var-args? [arg-names]
  (let [n (count arg-names)]
    (and (> n 1) (= (nth arg-names (- n 2)) (symbol "&")))))

(defn arity [arg-names]
  (let [n (count arg-names)]
    (if (var-args? arg-names)
      (- n 1)
      n)))

(defn args-of [arg-names args]
  (if (var-args? arg-names)
    (dbg args) ;TODO
    args))

(defn fn-app-no-arity [[arg-names impl] args env]
  (let [as (args-of arg-names args)]
    (assert (= (count arg-names) (count as)))
    (my-eval impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave arg-names as))))))



(defn correct-arity? [[arg-names impl] args]
  (= (arity arg-names) (count args))) 

(defn fn-app [f args env]
  (if (multiple-arity? f)
    (let [x (filter #(correct-arity? % args) f)]
      (assert (= (count x) 1))
      (-> x first (fn-app-no-arity args env))) 
    (fn-app-no-arity f args env)))

(def pre-def-map 
  {"+" + , "-" -, "=" =, ">" >, "<" <, "not" not})

(defn fn-predefined [name args env]
  (condp = name
    "quote" (first args) ;TODO is first correct here
    "unquote" (my-eval (my-eval (first args) env) env)
    "if" (if (my-eval (first args) env) (my-eval (nth args 1) env) (my-eval (nth args 2) env))
    "and" (reduce (fn [acc b] (and acc b)) (map #(my-eval % env) args))
    "or" (reduce (fn [acc b] (or acc b)) (map #(my-eval % env) args))
    "first" (first (map #(my-eval % env) (first args)))
    "rest" (rest (map #(my-eval % env) (first args)))
    "cons" (cons (-> args first (my-eval env)) (-> args second (my-eval env)))
    (if-let [f (pre-def-map name)]
      (apply f (map #(my-eval % env) args))
      (println (format "Symbol %s can't be found" name)))))

(defn my-apply [[name & args] env]
  (let [n (str name)]
    (condp = n
      "def" (swap! vars assoc (first args) (with-meta (-> args second (my-eval env)) {:macro (-> args rest second)}))
      "fn" args
      (if-let [the-fn (dbg (my-eval name env))] 
        (fn-app the-fn args env)
        (fn-predefined n args env))
        )))

(extend-protocol MyEval
  java.util.List (my-eval [expr env] (my-apply expr env))
  String (my-eval [s _] s)
  clojure.lang.Symbol (my-eval [s env] ((merge @vars env) s))
  Number (my-eval [n _] n)
  clojure.lang.Keyword (my-eval [k _] k)
  Boolean (my-eval [b _] b)
  
  )

(defmacro evl [expr]
  `(my-eval '~expr {}))



(evl (def >= (fn [v1 v2] (or (> v1 v2) (= v1 v2)))))
(evl (def <= (fn [v1 v2] (or (< v1 v2) (= v1 v2)))))
(evl (def abs (fn [v] (if (< v 0) (- v) v))))
(evl (def max (fn [v1 v2] (if (> v1 v2) v1 v2))))
(evl (def min (fn [v1 v2] (if (< v1 v2) v1 v2))))

(evl (def max
  (fn 
    ([x] x)
    ([x y] (. clojure.lang.Numbers (max x y)))
    ([x y & more]
     (reduce max (max x y) more)))))
