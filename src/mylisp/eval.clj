(ns mylisp.eval
  (:require [clojure.repl :refer [source doc]]))

(defmacro dbg[x] `(let [x# ~x] (println  '~x "=" x#) x#))

(defn with-meta-if [obj m]
  (if (instance? clojure.lang.IObj obj)
    (with-meta obj m)
    obj))

(defmacro def-map [& syms] (apply hash-map (mapcat (fn [v] [`'~v v]) syms)))
  
(def global-env (atom (def-map first rest empty? cons list + - < > = println)))

(defn self-eval? [expr] (some #(% expr) [string? number? keyword? nil? fn? (partial instance? Boolean)]))
      
(defn quoted? [[q]] (= q 'quote))

(defn def? [[d]] (= d 'def))

(defn if? [[i]] (= i 'if))

(defn lambda? [[l]] (= l 'fn))

(defn syntax-quote? [[sq]] (= sq 'syntax-quote))

(defn do? [[d]] (= d 'do)) 

(defn unquote? [[unquote] sq]
  (when (= unquote 'unquote)
    (if sq true (throw (IllegalStateException. "must be inside a syntax qoute")))))

(defn re-eval-of [sq kw]
  (if sq :re-eval kw))

(defmulti eval-expr 
  (fn [expr env sq]
    (cond 
      (self-eval? expr) :self
      (symbol? expr) (if sq :self :symbol)
      (vector? expr) :vector 
      (quoted? expr) (if sq :self :quoted)
      (syntax-quote? expr) :syntax-quote
      (unquote? expr sq) :unquote
      (def? expr) (re-eval-of sq :def) 
      (if? expr) (re-eval-of sq :if)
      (do? expr) (re-eval-of sq :do)
      (lambda? expr) (re-eval-of sq :lambda) 
      (seq? expr) (re-eval-of sq :app)
      )))

(defn multiple-arity? [f] (-> f first list?))

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
    (eval-expr impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave ans as))) sq)))

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

(defmethod eval-expr :self [expr _ sq] expr)

(defmethod eval-expr :symbol [expr env sq]
  (let [e (merge @global-env env)]
    (assert (contains? (into #{} (keys e)) expr) (format "Unable to resolve symnbol: %s in this context" expr))
    (e expr)))  

(defmethod eval-expr :quoted [[_ expr] _ sq] expr)

(defmethod eval-expr :def [[_ sym expr macro] env sq] (swap! global-env assoc sym (with-meta-if (eval-expr expr env sq) {:fn-type macro})))

(defmethod eval-expr :if [[_ pred alt1 alt2] env sq]
  (let [e #(eval-expr % env sq)]
    (if (e pred) (e alt1) (e alt2))))

(defmethod eval-expr :lambda [[_ & the-fn] _ _] the-fn)

(defn primitive-fn? [the-fn] (fn? the-fn))

(defmethod eval-expr :syntax-quote [[_ expr] env _] (eval-expr expr env true))

(defmethod eval-expr :re-eval [expr env _] (map #(eval-expr % env true) expr))

(defmethod eval-expr :vector [expr env sq] (mapv #(eval-expr % env sq) expr))

(defmethod eval-expr :unquote [[_ expr] env _]  (eval-expr expr env false))

(defn apply-primitive-fn [the-fn args] (apply the-fn args))

(defn macro? [the-fn] (= (-> the-fn meta :fn-type) :macro))

(defn apply-fn [the-fn args env sq]
  (let [m (macro? the-fn)
        ev-args (if m args (map #(eval-expr % env sq) args))
        res  (if (primitive-fn? the-fn)
               (apply-primitive-fn the-fn ev-args)
               (fn-app the-fn ev-args env sq))]
    (if m
      (eval-expr res env sq) ;TODO what should sq be here??
      res)))

(defmethod eval-expr :app [[the-fn & args] env sq] (apply-fn (eval-expr the-fn env sq) args env sq))


(defmethod eval-expr :do [[_ & exprs] env sq] 
  (doseq [expr (butlast exprs)] (eval-expr expr env sq))
  (when-let [l (last exprs)] (eval-expr l env sq)))

(defmacro evl [expr] `(eval-expr '~expr {} false))

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

(evl (def when (fn [pred alt] (syntax-quote (if (unquote pred) (unquote alt) nil))) :macro))

(evl 
  (def partition (fn [coll]
    (when (not (empty? coll))
      (if (empty? (rest (rest coll)))
        (cons [(first coll) (second coll)] (rest (rest coll)))
        (cons [(first coll) (second coll)] (partition (rest (rest coll)))))))
      ))

(evl (def dbg (fn [x] (syntax-quote  (println (unquote x) "=" (unquote x)))) :macro))

(evl 
  (def let1 (fn [pairs body]
              (when (not (empty? pairs))
                  (syntax-quote ((fn [(unquote (first (first pairs)))] (unquote (if (empty? (rest pairs)) body (let1 (rest pairs) body)))) (unquote (second (first pairs)))))))))
(evl 
  (def let (fn [assignments body]
    (let1 (partition assignments) body)) :macro))

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
