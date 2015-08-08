;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; examples of E4 usage (see README also) ;;;;;

;;I am not insist on this kind of E4 formatting,
;; it will take some time to get the best one or
;; we can stick to Forth guidelines
;;
;; all examples should be runnable with the
;; latest E4 version.

(xe4:set-options '(return-stack-after-eval . t)
		 '(flush-stack-before-eval . t))

;; example #1
;; immutable sequence 
(xe4:
 ( define sequence of "foo" "bar" "baz" )
 
 { reset
 { next "foo"
 { next "bar"
 { next "baz" } } } }
 { next sequence-reset }
       
 ( prints "for", "bar", "baz" )
 
 reset next .. next .. next ..
 ( next calls return the last value (baz) )
 
 reset
 ( and we can now use sequence-next again ))

;; example #2
;; self-restoring sequence (circilar list)
(xe4:
 { init { cycle head } }
 { head 1
 { cycle 2
 { cycle 3 init } } }
 
 init 
 cycle cycle cycle ( => 1, 2, 3 )
 cycle cycle cycle ( => 1, 2, 3 ))
 
;; example #3
;; simple iteration
(xe4:
 ( _ (underscore) is convention for lambdas )
 { times DUP 0 > IF _ 1- times ENDIF DROP }

 ( this solution works, but we can mess with counter at stack )
  
 { _ "hello, world!" .. } 6 times
 { _ DUP .. } 3 times)

;; example #4
;; loop for accumulate values
(xe4:
 ( previous loop can not be used to collect data into stack )
 ( but we can make another one, using our super-sequence )

 ( sequence consists of "foo", "bar" and "baz" )
 reset
 
 { iterate next "baz" = IF ELSE _ iterate ENDIF }
 { _ DUP 2 * }
 4 iterate .s ( => 4 8 16 ))
