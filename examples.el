;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; examples of E4 usage (see README also) ;;;;;

;; I am not insist on this kind of E4 formatting,
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

;; example #5
;; iterating over vector
(xe4:
 ( both items will be iteratable )
 { v [ 10 20 30 ] }
 { s "foxey" }

 { spine-length SWAP LEN ROT }             ( drop counter )
 { each spine-length DUP ROT < IF _ 1+ each ELSE DROP ENDIF }

 ( modify current index, set value of 97 + index )
 { _ DUP ROT SWAP DUP 97 + SET SWAP .s }

 v 0 each
 s 1 each) ; => ("fbcde" [97 98 99])

;;; the most recent added fuctions below

(xe4: 4 3 2 1 TUCK) ; => (1 2 1 3 4)
(xe4: 0 1 TUCK TUCK) ; => (1 0 1 1)

(xe4: 0 1 2 ROT) ; => (0 2 1)
(xe4: 0 1 2 ROT ROT ROT) ; => (2 1 0)

(xe4: [6 5] 1 NTH SWAP DROP) ; => (5)
(xe4: "speed" 0 NTH) ; => (115 "speed")

(xe4: [0 1 2] LEN) ; => (3 [0 1 2])
(xe4: "measure me!" LEN) ; => (11 "measure me")

(xe4: [0 1 2] SPLIT DROP) ; => (1 2)
(xe4: "qwe" SPLIT) ; => (113 119 101)

(xe4: 0 1 2 3 VEC) ; => ([0 1 2])
(xe4: 113 119 101 3 STR) ; => ("qwe")

(xe4: [0 0] 0 "foo" SET 1 "bar" SET) ; => (["foo" "bar"])
(xe4: "Aa" 0 ?a SET 1 ?A SET) ; => ("aA")

(xe4: [[8]] 0 NTH 0 NTH) ; => (8 [8] [[8]])

