;; examples of E4 usage

(xe4:set-options '(return-stack-after-eval . t)
		 '(flush-stack-before-eval . t))

(xe4:
 {
   stack-print
   DEPTH 0 > IF
   .. stack-print
   ENDIF
 }
 ( print 1, 2, 3, 2, 1 )
 1 2 3 2 1 stack-print DUP DUP DROP DROP)

(xe4:
 { fib 1- DUP 1 > IF DUP fib SWAP 1- fib + ENDIF }
 8 fib)

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

(xe4:
 { var "magic" { var 777 } }
 ( first call gives a "magic", the rest 2 return 777 )
 var var var)
