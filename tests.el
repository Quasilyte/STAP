(xe4:with-empty-stack
 (e4:
  { stack-print ( fd fd )
  DEPTH 0 > IF .. stack-print ENDIF }
  
  ( print 1, 2, 3, 2, 1 )
  1 2 3 2 1 stack-print DUP DUP DROP DROP))

(xe4:with-empty-stack
 (e4:
  { fib 1- DUP 1 > IF DUP fib SWAP 1- fib + ENDIF }
  8 fib))

(xe4:with-empty-stack
 (e4:
  ( words are used for both function and variable declarations )
  { foo 1 } { bar 10 }
  foo
  ( word can be reassigned )
  { foo bar }
  foo))

(xe4:with-empty-stack
 (e4:
  { var "magic" { var 777 } }
  ( first call gives a "magic", the rest 2 return 777 )
  var var var))
