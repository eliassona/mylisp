mylisp
======

Lisp interpreter.
Ever since I got into Lisp and more specifically Clojure I've been fascinated by the Lisp eval/apply functions. 
How can something so small be so powerful? 
Naturally I had to implement a Lisp interpreter myself. This way I'll hopefully understand why it's so powerful.

I especially like the macro part. It's just an extra condition in apply. By adding this condition you get a macro system!!


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
