(ns mylisp.eval)

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(def vars (atom {}))

(defprotocol MyEval
  (my-eval [expr env]))

(defn fn-app [[arg-names impl] args env]
  (assert (= (count arg-names) (count args)))
  (my-eval impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave arg-names args)))))


(def pre-def-map 
  {"+" + , "-" -})

(defn fn-predefined [name args env]
  (condp = name
    "quote" (first args) ;TODO is first correct here
    "unquote" (my-eval (my-eval (first args) env) env)
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
  clojure.lang.Keyword (my-eval [k _] k))

(defmacro evl [expr]
  `(my-eval '~expr {}))