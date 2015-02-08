mylisp
======

Lisp interpreter.
Ever since I got into Lisp and more specifically Clojure I've been fascinated by the Lisp eval/apply functions. 
How can something so small be so powerful? 
Naturally I had to implement a Lisp interpreter myself. This way I'll hopefully understand why it's so powerful.

I especially like the macro part. It's just an extra condition in apply. By adding this condition you get a macro system!!
The code is [here https://github.com/eliassona/mylisp/blob/master/src/mylisp/eval.clj]

The first 130 lines contains the interpreter. The following lines calls the interpreter to build an environment for the interpreter. Adding standard function and macros such as reduce etc. 

Line 31 to 44 contains the main case statement of the eval function

Line 46 - 80 deals with multiple arity

Line 86 to 129 is the implementation of eval/apply

Line 123 is the condition for macros

Below are some examples.
The main function of the interpreter is called 
```clojure 
evl
```

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
