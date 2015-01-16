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

(defn syntax-quote? [[sq]]
  (= sq 'syntax-quote))

(defn unquote? [[unquote] sq]
  (when (= unquote 'unquote)
    (if sq true (throw (IllegalStateException. "must be inside a syntax qoute")))))
  
  

(defn res-of [res sq]
  (if sq :self res))

(defmulti new-eval 
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

(defn fn-app-no-arity [[arg-names impl] args env sq]
  (let [[ans as] (args-of arg-names args)]
    (assert (= (count ans) (count as)))
    (new-eval impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave ans as))) sq)))

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
      (-> x first (fn-app-no-arity args env sq))) 
    (fn-app-no-arity f args env sq)))

(defmethod new-eval :self [expr _ sq] expr)

(defmethod new-eval :symbol [expr env sq]
  (let [e (merge @global-env env)]
    (assert (contains? (into #{} (keys e)) expr) (format "Unable to resolve symnbol: %s in this context" expr))
    (e expr)))  

(defmethod new-eval :quoted [[_ expr] _ sq] expr)

(defmethod new-eval :def [[_ sym expr] env sq] (swap! global-env assoc sym (new-eval expr env sq)))

(defmethod new-eval :if [[_ pred alt1 alt2] env sq]
  (let [e #(new-eval % env sq)]
    (if (e pred) (e alt1) (e alt2))))

(defmethod new-eval :lambda [expr _ sq] (rest expr))

(defn primitive-fn? [the-fn] (fn? the-fn))

(defmethod new-eval :syntax-quote [[_ expr] env _]
  (new-eval expr env true))

(defmethod new-eval :re-eval [expr env _]
  (map #(new-eval % env true) expr))

(defmethod new-eval :vector [expr env sq]
  (mapv #(new-eval % env sq) expr))

(defmethod new-eval :unquote [[_ expr] env _] 
  (new-eval expr env false))

(defn apply-primitive-fn [the-fn args] (apply the-fn args))

(defn new-apply [the-fn args env sq]
  (let [ev-args (map #(new-eval % env sq) args)]
    (cond 
      (primitive-fn? the-fn)
      (apply-primitive-fn the-fn ev-args)
      :else 
      (fn-app the-fn ev-args env sq))))

(defmethod new-eval :app [[the-fn & args] env sq]
  (new-apply (new-eval the-fn env sq) args env sq))

(defmacro evl [expr]
  `(new-eval '~expr {} false))

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

