(xe4:with-empty-stack
 (e4:
  { stack-print ( pop and print every element in the data stack )
  DEPTH 0 > IF .. stack-print ENDIF }

  ( print 1, 2, 3, 2 and 1 again )
  1 2 3 2 1 stack-print DUP DUP DROP DROP))

(xe4:with-empty-stack
 (e4:
  { fib 1- DUP 1 > IF DUP fib SWAP 1- fib + ENDIF }
  8 fib)) 

