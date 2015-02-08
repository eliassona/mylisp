mylisp
======

Lisp interpreter.
Ever since I got into Lisp and more specifically Clojure I've been fascinated by the Lisp eval/apply functions. 
How can something so small be so powerful? 
Naturally I had to implement a Lisp interpreter myself. This way I'll hopefully understand why it's so powerful.

I especially like the macro part. It's just an extra condition of apply. By adding this condition you get a macro system!!


The first 130 lines contains the interpreter. The following lines uses the interpreter to build an environment for the interpreter. Adding standard function and macros such as reduce etc. 

The code of the interpreter here 
[https://github.com/eliassona/mylisp/blob/master/src/mylisp/eval.clj]


Below are some examples.

The main function of the interpreter is called evl


```clojure
(use 'mylisp.eval)
(evl (+ 1 1))
2
```

Define a value
```clojure
(evl (def a 1))
nil
```

```clojure
(evl a)
1
```

Define a function
```clojure
(evl (def a-fn (fn [x] (+ x 1)))) 
nil
```

Call the function
```clojure
(evl (a-fn 1))
2
```

let is implemented as a macro using lambda
```clojure
(evl 
  (let [a 1
        b 2]
 (+ a b)))
3 
```
