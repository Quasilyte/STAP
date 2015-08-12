<h2>Stap -- STAck Processing language</h2>

<h3>Short description</h3>
Stap is a high level functional **Forth** dialect with a taste of lisp.<br>
Dynamically typed, containing only basic data structures and<br>
generic operations on them, language is super slim and easy to learn.<br>

<hr>
This is embedded **Emacs Lisp** implementation for **Stap**, but<br>
the latter is not bounded to it as we can freely make implementation<br>
for any platform (thanks to **Forth** roots, **Stap** is nearly syntaxless).<br>

<blockquote>
Because examples in README.md tend to become outdated too fast,<br>
look for some code inside `examples.el` or `tests.el`.<br>
</blockquote>

<h4>Warning!</h4>
**This is in active development, so anything can happen.**<br>
**Code you write today in Stap can fail with an error with tomorrow version!**

<h3>Navigation</h3>

*Points of interest:*

* [usage inside Emacs Lisp](etc/examples.el)
* [examples](etc/examples.stap)
* [predefined word set](docs/builtins.md)
* [quick guide into language](docs/LXinYM.md)

<h3>Emacs Stap Features</h3>

*Those features related only to this implementation:*

* inlining right into the Lisp code (full Emacs integration)
* utilities like stack rollbacks after multiple evaluations (state control)

<h3>Stap features</h3>

*We comparing Stap mostly to the Lisp and [G]Forth:*

* case-sensitive reader (most Forths are not)
* conditionals are not compile-only words
* nested definitions (delayed and partial compilation) 
* neat literals for scalars (strings, vectors) 
* homogeneous builtin data structures
* overloaded operators (+, =, etc)

<h3>Rationale</h3>
**Stap** grounded at Emacs because I believe it can help in some<br>
specific cases, when **Lisp** is kinda fails or when it makes you angry...<br>
<br>
The communication between **Stap** and **Emacs Lisp** is trivial,<br>
in fact, it is possible to create **Stap** code inside macros!<br>
<br>
And about perfomance: we are not far slower than hosting language.<br>
Maybe a bit slower.<br>
Your computer is so mighty anyway, do not feel sorry for CPU cycles!<br>
