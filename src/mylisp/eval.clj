(ns mylisp.eval
  (:require [clojure.repl :refer [source doc]]))

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(defn with-meta-if [obj m]
  (if (instance? clojure.lang.IObj obj)
    (with-meta obj m)
    obj))


(defmacro def-map [& syms] (apply hash-map (mapcat (fn [v] [`'~v v]) syms)))
  
(def global-env (atom (def-map first rest empty? list + - < > = println)))

(defn self-eval? [expr]
  (some #(% expr) [string? number? keyword? nil? fn? (partial instance? Boolean)]))
      
(defn quoted? [[q]] (= q 'quote))

(defn def? [[d]] (= d 'def))

(defn if? [[i]] (= i 'if))

(defn lambda? [[l]] (= l 'fn))

(defn syntax-quote? [[sq]]
  (= sq 'syntax-quote))

(defn unquote? [[unquote] sq]
  (when (= unquote 'unquote)
    (if sq true (throw (IllegalStateException. "must be inside a syntax qoute")))))
  
  

(defn res-of [res sq]
  (if sq :self res))

(defmulti my-eval 
  (fn [expr env sq]
    (cond 
      (self-eval? expr) :self
      (symbol? expr) (res-of :symbol sq)
      (vector? expr) :vector 
      (quoted? expr) (res-of :quoted sq)
      (syntax-quote? expr) :syntax-quote
      (unquote? expr sq) :unquote
      (def? expr) (if sq :re-eval :def)
      (if? expr) (if sq :re-eval :if)
      (lambda? expr) (if sq :re-eval :lambda) 
      (list? expr) (if sq :re-eval :app)
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

(defn fn-app-one-arity [[arg-names impl] args env sq]
  (let [[ans as] (args-of arg-names args)]
    (assert (= (count ans) (count as)))
    (my-eval impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave ans as))) sq)))

(defn correct-arity? [[arg-names impl] args]
  (let [an-n (arity arg-names)
        a-n (count args)]
    (if (var-args? arg-names)
      (<= an-n a-n)
      (= an-n a-n)))) 

(defn fn-app [f args env sq]
  (if (multiple-arity? f)
    (let [x (filter #(correct-arity? % args) f)]
      (assert (= (count x)) 1)
      (-> x first (fn-app-one-arity args env sq))) 
    (fn-app-one-arity f args env sq)))

(defmethod my-eval :self [expr _ sq] expr)

(defmethod my-eval :symbol [expr env sq]
  (let [e (merge @global-env env)]
    (assert (contains? (into #{} (keys e)) expr) (format "Unable to resolve symnbol: %s in this context" expr))
    (e expr)))  

(defmethod my-eval :quoted [[_ expr] _ sq] expr)

(defmethod my-eval :def [[_ sym expr macro] env sq] (swap! global-env assoc sym (with-meta-if (my-eval expr env sq) {:fn-type macro})))

(defmethod my-eval :if [[_ pred alt1 alt2] env sq]
  (let [e #(my-eval % env sq)]
    (if (e pred) (e alt1) (e alt2))))

(defmethod my-eval :lambda [expr _ sq] (rest expr))

(defn primitive-fn? [the-fn] (fn? the-fn))

(defmethod my-eval :syntax-quote [[_ expr] env _]
  (my-eval expr env true))

(defmethod my-eval :re-eval [expr env _]
  (map #(my-eval % env true) expr))

(defmethod my-eval :vector [expr env sq]
  (mapv #(my-eval % env sq) expr))

(defmethod my-eval :unquote [[_ expr] env _] 
  (my-eval expr env false))

(defn apply-primitive-fn [the-fn args] (apply the-fn args))

(defn macro? [the-fn]
  (= (-> the-fn meta :fn-type) :macro))

(defn my-apply [the-fn args env sq]
  (let [m (macro? the-fn)
        ev-args (if m args (map #(my-eval % env sq) args))
        res (cond 
              (primitive-fn? the-fn)
              (apply-primitive-fn the-fn ev-args)
              :else
              (fn-app the-fn ev-args env sq))]
    (if m 
      (my-eval res env sq) ;TODO what should sq be here??
      res)
    ))

(defmethod my-eval :app [[the-fn & args] env sq]
  (my-apply (my-eval the-fn env sq) args env sq))

(defmacro evl [expr]
  `(my-eval '~expr {} false))

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



