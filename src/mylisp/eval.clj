(ns mylisp.eval)

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(def vars (atom {}))

(defprotocol MyEval
  (my-eval [expr env]))

(defn fn-app [[arg-names impl] args env]
  (assert (= (count arg-names) (count args)))
  (my-eval impl (reduce (fn [acc [n v]] (assoc acc n v)) env (partition 2 (interleave arg-names args)))))

(defn fn-predefined [name args env]
  (apply 
    ({
       "+" +
       "-" -
      } name) (map #(my-eval % env) args)))

(defn my-apply [[name & args] env]
  (let [n (str name)]
    (condp = n
      "def" (swap! vars assoc (first args) (-> args second (my-eval env)))
      "fn" args
      (if-let [the-fn ((merge @vars env) name)] 
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