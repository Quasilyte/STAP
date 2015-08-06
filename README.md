<h2>E4 -- Emacs Forth</h2>

<h3>Short description</h3>
Emacs Forth is a partial Forth implementation written in lisp.<br>
It follows the FORTH-83 standard (required word set),<br>
but not in all aspects. <br>

<h3>Features</h3>
* inlining right into the lisp code (full Emacs integration)
* utilities like stack rollbacks after multiple evaluations
* case-sensitive reader (most Forths are not)
* conditionals are not compile-only words

<h3>Rationale</h3>
Because something can be better expressed in one language/style<br>
while other tasks are suited for something else well.<br>
<br>
Say we want to find `fib(n)` number.<br>
Forget about performance and think of fibonacci definition.<br>

We can express it in forth like that:
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

```elisp
;; just eval code and print resulting E4 stack:
(message "%s"
	 (e4: 4 4 + DUP *)) ; => "64"

;; now stack has "64" in it, but we can clear
;; it this way: (e4:stack-flush).
;; nah, it is boring, here is a better way:
(e4:with-empty-stack
 (e4: 
  2 2 =)) ; => '(-1) 

;; for now I am a bit lazy to write more comments,
;; but here is (at least) some more concrete examples:
(e4:
 { lucky ( -- n)  7 }
 lucky .. ( prints 7 )
 lucky DEPTH ..)

(e4:stack-flush)

(setq result (car (e4: 1000 10 / 10 /)))
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
    <td>dot (.) is by Emacs lisp interpreter to denote cons pairs</td>
  </tr>
  <tr>
    <td>recurse</td>
    <td>word symbol</td>
    <td>call word recursively</td>
    <td>because you want to call foo by foo, not by recurse</td>
  </tr>
</table>

<h3>Syntax</h3>
Just kidding, Forth has no syntax, right?<br>
