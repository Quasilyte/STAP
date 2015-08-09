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

(xe4:
 { func 1 2 3 }
 "func" SEE ( func: (1 2 3) )
 "concat" SEE ( word `concat' is not defined))

(xe4: 1 2 3 4 NIP) ; => (4 2 1)
(xe4: 1 2 3 4 NIP NIP) ; => (4 1)

(xe4: 4 3 2 1 TUCK) ; => (1 2 1 3 4)
(xe4: 0 1 TUCK TUCK) ; => (1 0 1 1)

(xe4: 1 2 3 4 OVER) ; => (3 4 3 2 1)
(xe4: 2 1 OVER OVER) ; => (1 2 1 2)

(xe4: 0 1 2 ROT) ; => (0 2 1)
(xe4: 0 1 2 ROT ROT ROT) ; => (2 1 0)

(xe4: [6 5] 1 NTH SWAP DROP) ; => (5)
(xe4: "speed" 0 NTH 1 STR) ; => ("s" "speed")

(xe4: [0 1 2] LEN) ; => (3 [0 1 2])
(xe4: "measure me!" LEN) ; => (11 "measure me")

(xe4: [0 1 2] SPLIT DROP) ; => (1 2)
(xe4: "qwe" SPLIT) ; => (113 119 101)

;; VEC and STR are highly versatile:

;; create sequence from stack elements
(xe4: 0 1 2 3 VEC) ; => ([2 1 0])
(xe4: ?0 ?1 ?2 3 STR) ; => ("210")

;; create sequence from sequence of another type:
(xe4: "012" VEC) ; => ([48 49 50])
(xe4: [48 49 50] STR) ; => ("012")

;; create empty sequences with n-capacity:
(xe4: -4 VEC) ; => ([0 0 0 0])
(xe4: -4 STR) ; => ("^@^@^@^@")

;; other operations on sequences:

(xe4: [0 0] 0 "foo" SET 1 "bar" SET) ; => (["foo" "bar"])
(xe4: "Aa" 0 ?a SET 1 ?A SET) ; => ("aA")

(xe4: [[8]] 0 NTH 0 NTH) ; => (8 [8] [[8]])

;;; extension from xe4 wordset:

(xe4:import-extra-wordset)

(xe4: 0 0= 2 0=) ; => (0 -1)
(xe4: 2 0> -1 0>) ; => (0 -1)
(xe4: -1 0< 2 0<) ; => (0 -1)

(xe4: 7 2+ 9 2-) ; => (7 9)

(xe4: 5 MAKE-VEC) ; => ([0 0 0 0 0])
(xe4: 2 MAKE-STR) ; => ("^@^@")

(xe4: 0 1 PAIR) ; => ([1 0])
(xe4: 0 1 PAIR 1ST SWAP 2ND) ; => (0 [1 0] 1)
