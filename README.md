<h2>E4 -- Emacs Forth</h2>

<h3>Short description</h3>
Emacs Forth is a partial Forth implementation written in lisp.<br>
It follows the FORTH-83 standard (required word set),<br>
but not in all aspects. <br>

<h3>Features</h3>
* inlinable right into the lisp code (full Emacs integration)
* utilities like stack rollbacks after multiple evaluations

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
</table>

<h3>Syntax</h3>
Just kidding, Forth has no syntax, right?<br>
