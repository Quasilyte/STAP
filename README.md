<h2>E4 -- Emacs Forth</h2>

<h3>Short description</h3>
Emacs Forth is a partial Forth implementation written in lisp.<br>
It follows the FORTH-83 standard (required word set),<br>
but not in all aspects. <br>

<h3>Features</h3>
* inlinable right into the lisp code (full Emacs integration)
* utilities like stack rollbacks after multiple evaluations

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
</table>

<h3>Syntax</h3>
Just kidding, Forth has no syntax, right?<br>
