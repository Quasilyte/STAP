<h3>STAP predefined word set</h3>

<h4>Data types</h4>

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
    <td>pop</td>
    <td>( a1 -- )</td>
    <td>
      takes top stack element and stores it until next `pop`
      override the value
    </td>
  </tr>

  <tr>
    <td>push</td>
    <td>( -- a1 )</td>
    <td>
      puts stored (after last `pop`) element on top
      of the stack
    </td>
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
    <td>( v1 n1 -- ? )</td>
    <td>
      removes `n1` elements from stack, applies
      `v1` transformation using deleted elements
    </td>
  </tr>
<table>

<h4>Printing operations</h4>

<table>
  <tr>
    <th>Symbol</th>
    <th>Signature</th>
    <th>Description</th>
  </tr>

  <tr>
    <td>@one</td>
    <td>( a1 -- )</td>
    <td>
      drops top stack element and prints its value
      to the *Messages* buffer
    </td>
  </tr>

  <tr>
    <td>@all</td>
    <td>( -- )</td>
    <td>
      without modifying the stack prints its current
      size plus all contained values
    </td>
  </tr>

  <tr>
    <td>@describe</td>
    <td>( s1 -- )</td>
    <td>
      lookup for word in dictionary by given string and print
      the result
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
    <td>vec(convert)</td>
    <td>( s1 -- v1 )</td>
    <td>
      convert string `s1` into `v1` vector
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
    <td>str(convert)</td>
    <td>( v1 -- s1 )</td>
    <td>
      convert vector `v1` into `s1` string
    </td>
  </tr>
<table>