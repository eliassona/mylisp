(ns mylisp.eval
  (:require [clojure.repl :refer [source doc]]))

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(defn with-meta-if [obj m]
  (if (instance? clojure.lang.IObj obj)
    (with-meta obj m)
    obj))

(defmacro def-map [& syms] `~(apply hash-map (mapcat (fn [v] [`'~v v]) syms)))
  
(def global-env (atom (def-map first rest empty? list + - < > = println)))

(defn self-eval? [expr]
  (some #(% expr) [string? number? keyword? nil? fn? (partial instance? Boolean)]))
      
(defn quoted? [[q]] (= q 'quote))

(defn def? [[d]] (= d 'def))

(defn if? [[i]] (= i 'if))

(defn lambda? [[l]] (= l 'fn))

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
  (let [an-n (arity arg-names)
        a-n (count args)]
    (if (var-args? arg-names)
      (<= an-n a-n)
      (= an-n a-n)))) 

(defn fn-app [f args env]
  (if (multiple-arity? f)
    (let [x (filter #(correct-arity? % args) f)]
      (assert (= (count x)) 1)
      (-> x first (fn-app-no-arity args env))) 
    (fn-app-no-arity f args env)))

(defmethod new-eval :self [expr _] expr)

(defmethod new-eval :symbol [expr env]
  (let [e (merge @global-env env)]
    (assert (contains? (into #{} (keys e)) expr) (format "Unable to resolve symnbol: %s in this context" expr))
    (e expr)))  

(defmethod new-eval :quoted [[_ expr] _] expr)

(defmethod new-eval :def [[_ sym expr] env] (swap! global-env assoc sym (new-eval expr env)))

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

(defmacro evl [expr]
  `(new-eval '~expr {}))

;;----------------------------------------------------------------------------------------------------------------------

;;some standard functions
 
(evl (def >= (fn [v1 v2] (or (> v1 v2) (= v1 v2)))))
(evl (def <= (fn [v1 v2] (or (< v1 v2) (= v1 v2)))))
(evl (def abs (fn [v] (if (< v 0) (- v) v))))
(evl (def count (fn [coll] (if (empty? coll) 0 (+ (count (rest coll)) 1)))))
(evl (def second (fn [coll] (first (rest coll)))))
(evl (def not (fn [x] (if x false true))))
(evl (def not= (fn 
                 ([x] false)
                 ([x y] (not (= x y)))
                 ([x y & more] (not (apply = x y more))))))

#_(evl (def and 
        (fn  
          ([] true)
          ([x] x)
          ([x & next]
           `(let [and# ~x]
              (if and# (and ~@next) and#))))) :macro)

(evl (def max
  (fn 
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more]
     (reduce max (max x y) more)))))
(evl (def min
  (fn 
    ([x] x)
    ([x y] (if (< x y) x y))
    ([x y & more]
     (reduce min (min x y) more)))))
(evl 
  (def reduce (fn [f val coll]
    (if (empty? coll)
      val
      (reduce f (f val (first coll)) (rest coll))))))

