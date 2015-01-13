(ns mylisp.eval
  (:require [clojure.repl :refer [source doc]]))

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(def vars (atom {'first first, 'rest rest 'empty? empty?, '+ +, '- -, 'println println}))


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
    (let [argsv (vec args)
          ix (- (count arg-names) 2)
          svf #(subvec % 0 ix)]
      [(conj (svf arg-names) (last arg-names))
       (conj (svf argsv) (subvec argsv ix))]) 
    [arg-names args]))

(defn fn-app-no-arity [[arg-names impl] args env]
  (let [[ans as] (args-of arg-names args)]
    (assert (= (count ans) (count as)))
    (my-eval impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave ans as))))))

(defn correct-arity? [[arg-names impl] args]
  (= (arity arg-names) (count args))) 

(defn fn-app [f args env]
  (if (multiple-arity? f)
    (let [x (filter #(correct-arity? % args) f)]
      (assert (= (count x) 1))
      (-> x first (fn-app-no-arity args env))) 
    (fn-app-no-arity f args env)))

(def pre-def-map 
  {"+" + , "-" -, "=" =, ">" >, "<" <, "not" not, "println" println})

(defn fn-predefined [name args env]
  (condp = name
    "quote" (first args) ;TODO is first correct here
    "unquote" (my-eval (my-eval (first args) env) env)
    "if" (if (my-eval (first args) env) (my-eval (nth args 1) env) (my-eval (nth args 2) env))
    "and" (reduce (fn [acc b] (and acc b)) (map #(my-eval % env) args))
    "or" (reduce (fn [acc b] (or acc b)) (map #(my-eval % env) args))
    "first" (first (my-eval (first args) env))
    "rest" (rest (map #(my-eval % env) (my-eval (first args) env)))
    "empty?" (empty? (my-eval (first args) env))
    "cons" (cons (-> args first (my-eval env)) (-> args second (my-eval env)))
    (if-let [f (pre-def-map name)]
      (apply f (map #(my-eval % env) args))
      (println (format "Symbol %s can't be found" name)))))

(defn with-meta-if [obj m]
  (if (instance? clojure.lang.IObj obj)
    (with-meta obj m)
    obj))

(defn my-apply [[name & args] env]
  (let [n (str name)]
    (condp = n
      "def" (swap! vars assoc (first args) (with-meta-if (-> args second (my-eval env)) {:macro (-> args rest second)}))
      "fn" args
      (if-let [the-fn (my-eval name env)] 
        (fn-app the-fn args env)
        (fn-predefined n args env))
        )))

(extend-protocol MyEval
  clojure.lang.IPersistentList (my-eval [expr env] (my-apply expr env))
  String (my-eval [s _] s)
  clojure.lang.Symbol (my-eval [s env] ((merge @vars env) s))
  Number (my-eval [n _] n)
  clojure.lang.Keyword (my-eval [k _] k)
  Boolean (my-eval [b _] b)
  
  )

(defmacro evl [expr]
  `(my-eval '~expr {}))

;;------------------------------------

(defn self-eval? [expr]
  (some #(% expr) [string? number? keyword? nil? fn?]))
      
(defn quoted? [[q]]
  (= q 'quote))

(defn def? [[d]]
  (= d 'def))
(defn if? [[i]]
  (= i 'if))
(defn lambda? [[l]]
  (= l 'fn))

(defmulti new-eval 
  (fn [expr env]
    (cond 
      (self-eval? expr) :self
      (symbol? expr) :symbol
      (quoted? expr) :quoted
      (def? expr) :def
      (if? expr) :if
      (lambda? expr) :lambda
      (list? expr) :app
    )))

(defmethod new-eval :self [expr _] 
  expr)

(defmethod new-eval :symbol [expr env] 
  (my-eval (@vars (dbg expr)) env) ) 


(defmethod new-eval :quoted [[_ expr] _] 
  expr)

(defmethod new-eval :def [[_ sym expr] _] 
  (swap! vars assoc sym expr))

(defmethod new-eval :if [[pred alt1 alt2] env]
  (let [e #(my-eval % env)]
    (if (e pred) (e alt1) (e alt2))))
(defmethod new-eval :lambda [expr _] 
  "lambda")

(defn primitive-fn? [the-fn]
  (fn? the-fn))
  
(defn compound-fn? [the-fn]
  false
  )
(defn apply-primitive-fn [the-fn args]
  (apply the-fn args))

(defn new-apply [the-fn args env]
  (let [ev-args (map #(new-eval % env) args)]
    (cond 
      (primitive-fn? the-fn)
      (apply-primitive-fn the-fn ev-args)
      (compound-fn? the-fn)
      (dbg the-fn))))
    

(defmethod new-eval :app [[the-fn & args] env]
  (new-apply (my-eval the-fn env) args env))

  



;;some standard functions

(evl (def >= (fn [v1 v2] (or (> v1 v2) (= v1 v2)))))
(evl (def <= (fn [v1 v2] (or (< v1 v2) (= v1 v2)))))
(evl (def abs (fn [v] (if (< v 0) (- v) v))))
(evl (def max (fn [v1 v2] (if (> v1 v2) v1 v2))))
(evl (def min (fn [v1 v2] (if (< v1 v2) v1 v2))))
(evl (def count (fn [coll] (if (empty? coll) 0 (+ (count (rest coll)) 1)))))
(evl (def second (fn [coll] (first (rest coll)))))
(evl (def max
  (fn 
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more]
     3))))


(defn co [coll]
  (if (empty? coll)
    0
    (+ (co (rest coll)) 1)))
  

(defn rd [f val coll]
  (if (= (count coll) 0)
    val
    (rd f (f val (first coll)) (rest coll))))
  
    
