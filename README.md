<h2>E4 -- Emacs Forth</h2>

<h3>Short description</h3>
Emacs Forth is a partial Forth implementation written in lisp.<br>
It uses some of the FORTH-83 standard (required word set)<br>
ideas, but our Forth is far Lispier. <br>

<h3>Features</h3>
* inlining right into the Lisp code (full Emacs integration)
* utilities like stack rollbacks after multiple evaluations (state control)
* case-sensitive reader (most Forths are not)
* conditionals are not compile-only words
* [almost] stackless recursion
* nested definitions (delayed and partial compilation)
* neat literals for scalars (strings, vectors) thanks to lisp

<h3>Rationale</h3>
Because something can be expressed better in one language/style<br>
while other tasks are suited for something else.<br>
<br>
Say we want to find `fib(n)` number.<br>
Forget about performance and think of fibonacci definition.<br>

We can express it in Forth like that:
```forth
\ readable even in form of 1-liner
: fib 1- DUP 1 > IF DUP RECURSE SWAP 1- RECURSE + ENDIF ;
\ call & print
10 fib .
```

Translate to lisp:
```elisp
(defun fib (n)
  (let ((n (1- n)))
    (if (> n 1)
	(+ (fib n)
	   (fib (1- n)))
      n)))

;; we can write this in 1 line and end up in unreadable code:
(defun fib (n) (let ((n (1- n))) (if (> n 1) (+ (fib n) (fib (1- n))) n)))

;; call & print
(message "%s" (fib 10))
```

E4 to the rescue:
```elisp
;; define
(e4: { fib 1- DUP 1 > IF DUP fib SWAP 1- fib + ENDIF })
;; call & print
(e4: 10 fib ..)
```

<h3>Examples</h3>
Check out `tests.el` to see more.<br>


```elisp
;; instead of using plain `e4:', you should use `xe4:'
;; modify options for E4 execution via `xe4:'
(xe4:set-options '(return-stack-after-eval . t)
		 '(flush-stack-before-eval . t))

;; just eval code and print resulting E4 stack:
(message "%s"
	 (xe4: 4 4 + DUP *)) ; => "64"

;; remember we turned on stack returning?
;; here we get a list of 1 elements
(xe4: 2 2 =) ; => '(-1) 

;; we can pop and print top stack element by `..'
(xe4:
 { lucky ( -- n)  777 }
 lucky .. ( prints 777 )
 lucky DEPTH .. ( prints 1 ))

;; to print entire stack and its length (w/o flush)
(xe4: 1 2 3 4 .s)

;; we can print and flush stack with this recursive word
(xe4:
 {
   .s+flush
   DEPTH 0 > IF
   .. .s+flush
   ENDIF
 }
 ( print 1, 2, 3, 2, 1 )
 1 2 3 2 1 .s+flush)

;; the { } definitions are flexible and are used for
;; variables, functions and lambdas
(xe4:
 ( words are used for both function and variable declarations )
 { foo 1 } { bar 10 }

 foo

 ( word can be reassigned )
 { foo bar }
 { bar 20 }
 
 ( but such an assignment is merely a substitution,
   not a copy, so foo returns 20 as its value )
 foo)

;; using { } to create nested definitions
(xe4:
 { var "magic" { var 777 } }
 ( first call gives a "magic", the rest 2 return 777 )
 var var var)

;; and it is really easy to use results from E4 in calling code
(setq result (car (xe4: 1000 10 / 10 /)))
result ; => 10
```

<h3>Words</h3>
Easiest way is to compare E4 with GForth.<br>
The words not listed here are either not implemented<br>
or equal to the GForth in both semantics and spelling.<br>

<table>
  <tr>
    <th>E4</th>
    <th>GForth</th>
    <th>Description</th>
    <th>Reason</th>
  </tr>
  <tr>
    <td>:</td>
    <td>{</td>
    <td>exit interpretation mode, start compiling</td>
    <td>colon character is OK, but { is chosen for consistency with }</td>
  </tr>
  <tr>
    <td>;</td>
    <td>}</td>
    <td>exit compilation mode, start interpreting</td>
    <td>because ; is used for comments in lisp and \; is plain ugly</td>
  </tr>
  <tr>
    <td>.</td>
    <td>..</td>
    <td>exit compilation mode, start interpreting</td>
    <td>dot (.) is used by Emacs lisp interpreter to denote cons pairs</td>
  </tr>
  <tr>
    <td>recurse</td>
    <td>by name</td>
    <td>call word recursively</td>
    <td>because you want to call foo by foo (e.g. by name), not by recurse (keyword)</td>
  </tr>
</table>

<h3>Syntax</h3>
Just kidding, Forth has no syntax, right?<br>
