(ns mylisp.eval
  (:require [clojure.repl :refer [source doc]]))

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(def vars (atom {'first first, 
                 'rest rest 
                 'empty? empty?, 
                 '+ +, 
                 '- -, 
                 'println println
                 '> >
                 '< <
                 '= =
                 }))

(defn with-meta-if [obj m]
  (if (instance? clojure.lang.IObj obj)
    (with-meta obj m)
    obj))

(defn self-eval? [expr]
  (some #(% expr) [string? number? keyword? nil? fn? (partial instance? Boolean)]))
      
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


(defn multiple-arity? [f]
  (-> f first list?))

(defn var-args? [arg-names]
  (let [n (count arg-names)]
    (and (> n 1) (= (nth arg-names (- n 2)) '&))))

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
    (new-eval impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave ans as))))))

(defn correct-arity? [[arg-names impl] args]
  (= (arity arg-names) (count args))) 

(defn fn-app [f args env]
  (if (multiple-arity? f)
    (let [x (filter #(correct-arity? % args) f)]
      (assert (= (count x) 1))
      (-> x first (fn-app-no-arity args env))) 
    (fn-app-no-arity f args env)))


(defmethod new-eval :self [expr _] 
  expr)

(defmethod new-eval :symbol [expr env] 
  (new-eval ((merge @vars env) expr) env) ) 


(defmethod new-eval :quoted [[_ expr] _] expr)

(defmethod new-eval :def [[_ sym expr] _] (swap! vars assoc sym expr))

(defmethod new-eval :if [[_ pred alt1 alt2] env]
  (let [e #(new-eval % env)]
    (if (e pred) (e alt1) (e alt2))))

(defmethod new-eval :lambda [expr _] (rest expr))

(defn primitive-fn? [the-fn] (fn? the-fn))
  

(defn apply-primitive-fn [the-fn args] (apply the-fn args))

(defn new-apply [the-fn args env]
  (let [ev-args (map #(new-eval % env) args)]
    (cond 
      (primitive-fn? the-fn)
      (apply-primitive-fn the-fn ev-args)
      :else 
      (fn-app the-fn ev-args env))))
    

(defmethod new-eval :app [[the-fn & args] env]
  (new-apply (new-eval the-fn env) args env))

;;----------------------------------------------------------------------------------------------------------------------

;;some standard functions
(comment 
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
  
    
)