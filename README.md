mylisp
======

Lisp interpreter.
Ever since I got into Lisp and more specifically Clojure I've been fascinated by the Lisp eval/apply functions. 
How can something so small be so powerful? 
Naturally I had to implement a Lisp interpreter myself. This way I'll hopefully understand why it's so powerful.

I especially like the macro part. It's just an extra condition in apply. By adding this condition you get a macro system!!


Below are some examples.
The main function of the interpreter is called evl.

(use 'mylisp.eval)

(evl (+ 1 1))

2

;define a value

(evl (def a 1))

nil

(evl a)

1


;define a function

(evl (def a-fn (fn [x] (+ x 1)))) 

nil

(evl (a-fn 1))

2

;let is implemented as a macro using lambda

(evl 
  (let [a 1
        b 2]
 (+ a b)))

3 








