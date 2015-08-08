;; examples of E4 usage (see README also)

(xe4:set-options '(return-stack-after-eval . t)
		 '(flush-stack-before-eval . t))

(xe4:
 { fib 1- DUP 1 > IF DUP fib SWAP 1- fib + ENDIF }
 8 fib)
