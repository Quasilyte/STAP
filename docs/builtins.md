<h3>STAP predefined word set</h3>

**Current word count: 17**

All those words are required to be shipped with particular<br>
implementation as builtins.<br>
<br>
Implementer can add some more words for user convenience,<br>
but prefered way is to define them inside language as standard library<br>
and by extending available inputs for `query` word.<br>
<br>
While it seems like an agressive rule, this should help to<br>
isolate standard specified and implementation-specific features.<br>
For example, `describe` is external command mostly because<br>
what it actually prints can depend on internal representation of<br>
STAP code and programmer should look at real picture rather than<br>
convenient, but falsy image.

<h4>Type Legend</h4>

Those abbreviations are used alongside whole document.

<table>
  <tr>
    <th>Marker</th>
    <th>Meaning</th>
  </tr>

  <tr>
    <td> a </td>
    <td>anything. Element of any type</td>
  </tr>

  <tr>
    <td> sv </td>
    <td>sequential type (string or vector)</td>
  </tr>

  <tr>
    <td> s </td>
    <td>string. Sequence of ASCII characters</td>
  </tr>

  <tr>
    <td> v </td>
    <td>vector. Fixed size array which can contain anything</td>
  </tr>

  <tr>
    <td> n </td>
    <td>number. Decimal integer or floating point value of double precision</td>
  </tr>

  <tr>
    <td> ? </td>
    <td>hard to predict (depends on input/side-effects)</td>
  </tr>
</table>

<h4>Stack Manipulations</h4>

<table>
  <tr>
    <th>Symbol</th>
    <th>Signature</th>
    <th>Description</th>
  </tr>
  
  <tr>
    <td>count</td>
    <td>( -- n1 )</td>
    <td>
      returns a number representing how much elements
      stack is containing at this moment
    </td>
  </tr>

  <tr>
    <td>shake</td>
    <td>( s1 n1 -- ? )</td>
    <td>
      removes `n1` elements from stack, applies
      `s1` transformation using deleted elements
    </td>
  </tr>

  <tr>
    <td>stash</td>
    <td>( -- v1 )</td>
    <td>
      places persistent vector (stash) `v1` on top of the stack.
      it can be used as a second stack.
    </td>
  </tr>
<table>

<h4>Conditionals</h4>

<table>
  <tr>
    <th>Symbol</th>
    <th>Signature</th>
    <th>Description</th>
  </tr>

  <tr>
    <td>if</td>
    <td>( a1 -- )</td>
    <td>
      checks top stack element againts 0 and if
      it is matched, skips everything until else/endif
    </td>
  </tr>

  <tr>
    <td>else</td>
    <td>( -- )</td>
    <td>
      evaluates tokens until endif or another else
    </td>
  </tr>

  <tr>
    <td>endif</td>
    <td>( -- )</td>
    <td>
      acts as a terminating word for the described
      above words
    </td>
  </tr>
<table>

<h4>Sequence Operations</h4>

<table>
  <tr>
    <th>Symbol</th>
    <th>Signature</th>
    <th>Description</th>
  </tr>

  <tr>
    <td>nth</td>
    <td>( sv1 n1 -- sv1 a1 )</td>
    <td>
      returns n-th (n1) element of the given
      sequence `sv1`
    </td>
  </tr>

  <tr>
    <td>len</td>
    <td>( sv1 -- sv1 n1 )</td>
    <td>
      pushes `sv1` size to stack.
      size is length for strings, count for vectors
    </td>
  </tr>

  <tr>
    <td>set</td>
    <td>( sv1 n1 a1 -- sv1 )</td>
    <td>
      update `sv1` sequence value at `n1` index
      using `a1` value
    </td>
  </tr>
  
  <tr>
    <td>split</td>
    <td>( sv1 -- ? )</td>
    <td>
      unroll sequence into separate scalars and
      put them all at stack
    </td>
  </tr>

  <tr>
    <td>vec(make)</td>
    <td>( ? n1 -- v1 )</td>
    <td>
      create a new vector `v1` from `n1` values
      lying at stack top
    </td>
  </tr>
  
  <tr>
    <td>vec(zeros)</td>
    <td>( n1 -- v1 )</td>
    <td>
      negative `n1` denotes how many 0 elements
      should occupy new vector `v1`
    </td>
  </tr>
  
  <tr>
    <td>vec(concat)</td>
    <td>( v1 v2 -- v3 )</td>
    <td>
      return the result of appending `v1` to `v2`
    </td>
  </tr>
  
  <tr>
    <td>str(make)</td>
    <td>( ? n1 -- v1 )</td>
    <td>
      create a new string `s1` from `n1` characters
      lying at stack top
    </td>
  </tr>
  
  <tr>
    <td>str(zeros)</td>
    <td>( n1 -- s1 )</td>
    <td>
      negative `n1` denotes how long new \0 filled
      string `s1` should be 
    </td>
  </tr>

  <tr>
    <td>str(concat)</td>
    <td>( s1 s2 -- s3 )</td>
    <td>
      return the result of appending `s1` to `s2`
    </td>
  </tr>
<table>

<h4>Environment Communication</h4>

<table>
  <tr>
    <th>Symbol</th>
    <th>Signature</th>
    <th>Description</th>
  </tr>

  <tr>
    <td>query</td>
    <td>( s1 -- ? )</td>
    <td>
      passes `s1` request to the interpreter.
      depending on query, may or may not return something
    </td>
  </tr>

  <tr>
    <td>rename</td>
    <td>( s1 s2 -- )</td>
    <td>
      if entry named `s1` exists in dictionary,
      it gets new name `s2`
    </td>
  </tr>
<table>

<h4>Type Assertions</h4>

<table>
  <tr>
    <th>Symbol</th>
    <th>Signature</th>
    <th>Description</th>
  </tr>

  <tr>
    <td>num!</td>
    <td>( a1 -- n1 )</td>
    <td>
      given `a1` tries to return it as a number.
      if it fails, program terminates with error message.
    </td>
  </tr>
  
  <tr>
    <td>str!</td>
    <td>( a1 -- s1 )</td>
    <td>
      given `a1` tries to return it as a string.
      if it fails, program terminates with error message.
    </td>
  </tr>
  
  <tr>
    <td>vec!</td>
    <td>( a1 -- v1 )</td>
    <td>
      wraps `a1` element in a vector if and only if it
      is not vector already. never fails.
    </td>
  </tr>
<table>

