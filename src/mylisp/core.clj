(ns mylisp.core
  (:use [clojure.pprint])
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]))

(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(defn grammar []
     "
     PROGRAM = EXPR*  
     EXPR = OPTIONAL-SPACE (SINGLE-LINE-COMMENT / NUMBER / (STRING |  LIST | VECTOR | MAP | SYMBOL | KEYWORD | (READER-MACRO | (SPECIAL-CHARS EXPR))))
     SPECIAL-CHARS = QUOTE | UNQUOTE | UNQUOTE-SPLICING | DEREF | SYNTAX-QUOTE
     UNQUOTE = <'~'>
     UNQUOTE-SPLICING = <'~@'>
     QUOTE = <'\\''>
     SYNTAX-QUOTE = <'`'>
     DEREF = <'@'>
     READER-MACRO = <'#'> (SET | LIST | ('_' EXPR))
     LIST = (<'('> OPTIONAL-SPACE <')'>) |  (<'('> ARGS <')'>)
     VECTOR = (<'['> OPTIONAL-SPACE <']'>) |  (<'['> ARGS <']'>)
     MAP = (<'{'> OPTIONAL-SPACE <'}'>) |  (<'{'> ARGS <'}'>)
     SET = (<'{'> OPTIONAL-SPACE <'}'>) |  (<'{'> ARGS <'}'>)
     <ARGS> = (EXPR SPACE)* EXPR
     KEYWORD = <':'> SYMBOL
     SYMBOL = #'[^\\'`~@\\(\\)\\[\\]{}:;#,\\^ \t\n0123456789]' #'[^\\'`~@\\(\\)\\[\\]{}:;,\\^ \t\n]*'
     SINGLE-LINE-COMMENT = <';'> #'.'* NEW-LINE 
     STRING = <'\\\"'> #'([^\"\\\\]|\\\\.)*' <'\\\"'>
     NUMBER = (INTEGER | FLOAT | FRACTION) | (SIGN (INTEGER | FLOAT | FRACTION))
     SIGN = (PLUS-SIGN | MINUS-SIGN)
     <PLUS-SIGN> = '+'
     <MINUS-SIGN> = '-' 
     FLOAT = (DEC-INT <'.'> DEC-INT) | 
             (DEC-INT <'.'>)
     INTEGER = DEC-INT | HEX-INT 
     FRACTION = INTEGER <'/'> INTEGER
     DEC-INT = #'[0-9]+'
     HEX-INT = <'0'> <'x' | 'X'> #'[0-9a-fA-F]+'
     NEW-LINE = '\n' | Epsilon
     <SPACE> = <#'[ \t\n,]+'>
     <OPTIONAL-SPACE> = <#'[ \t\n,]*'>
     ")

(defn parser
  "The parser"
  ([text start] (insta/parse (insta/parser (grammar) :start start) text))
  ([text] (mylisp->ast text :PROGRAM)))



(defn expr-fn [& args]
  (match 
    (vec args)
	     [[:SPECIAL-CHARS [sc]] expr]
       (condp = sc
         :QUOTE `'~expr
         :UNQUOTE `(unquote ~expr)
         :UNQUOTE-SPLICING `(unquote-splicing ~expr)
         :DEREF `(deref ~expr)
         :SYNTAX-QUOTE (read-string (str "`" (pr-str expr)))
         )
       [v c]
       v
	     [v]
	     v))

(defn number-fn [& args]
  (match (vec args)
    ["-" v] (- v)
    ["+" v] v
    [v] v)) 


(defn float-fn 
  ([v] (float-fn v 0))
  ([v1 v2] (read-string (format "%s.%s" v1 v2))))

(defn reader-macro-fn 
  ([expr]
    (cond 
      (instance? java.util.Set expr) expr
      (instance? java.util.List expr) (read-string (format "#%s" (pr-str expr)))))
  ([token expr]
    (condp = token
      "_"
      `(comment ~expr))))

(def ast->clj-map 
  {:SYMBOL (fn [& args] (-> str (apply args) symbol)) 
   :DEC-INT read-string
   :INTEGER identity
   :NUMBER number-fn
   :SIGN identity
   :EXPR expr-fn
   :LIST (fn [& args] `~args)
   :VECTOR (fn [& args] `~(vec args))
   :FLOAT float-fn
   :MAP (fn [& args] (apply hash-map args))
   :SET (fn [& args] (into #{} args))
   :READER-MACRO reader-macro-fn
   :FRACTION (fn [v1 v2] (/ v1 v2))
   :STRING identity
   :SINGLE-LINE-COMMENT (fn [& args] nil)
   :NEW-LINE (fn [])
   :KEYWORD keyword
   :PROGRAM (fn [& args] `(do ~@args))
   :HEX-INT (fn [expr] (->> expr (format "0x%s") read-string))  
   }
  )

(defn ast->clj [ast]
  "Transform an AST to Clojure"
  (insta/transform
    ast->clj-map 
    ast))

(defn my-eval [text]
  "Evaluate an expresion"
  (->  text parser ast->clj eval))
