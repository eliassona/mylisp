(ns mylisp.eval)

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(def vars (atom {}))

(defprotocol MyEval
  (my-eval [expr env]))

(defn fn-app [[arg-names impl] args env]
  (assert (= (count arg-names) (count args)))
  (my-eval impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave arg-names args)))))

(defmacro apply-macro [name args env]
 `(~(symbol name) ~@args)
 )

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
    (if-let [f (pre-def-map name)]
      (apply f (map #(my-eval % env) args))
      (println (format "Symbol %s can't be found" name)))))

(defn my-apply [[name & args] env]
  (let [n (str name)]
    (condp = n
      "def" (swap! vars assoc (first args) (-> args second (my-eval env)))
      "fn" args
      (if-let [the-fn (my-eval name env)] 
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
