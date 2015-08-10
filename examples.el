;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; examples of STAP usage ;;;;;

;; I am not insist on this kind of STAP formatting,
;; it will take some time to get the best one or
;; we can stick to Forth guidelines
;;
;; all examples should be runnable with the
;; latest STAP version.

;; we really need this one
(xstap:import-essential-dict)

(xstap:import-math-dict)

;; example #1
;; immutable sequence 
(xstap:
 ( define sequence of "foo" "bar" "baz" )
 
 { reset
   { next "foo"
   { next "bar"
   { next "baz" } } } }
   { next sequence-reset }
       
 ( prints "for", "bar", "baz" )
 
 reset next @one next @one next @one
 ( next calls return the last value (baz) )
 
 reset
 ( and we can now use sequence-next again ))

;; example #2
;; self-restoring sequence (circilar list)
(xstap:
 { init { cycle head } }
 { head 1
 { cycle 2
 { cycle 3 init } } }
 
 init 
 cycle cycle cycle ( => 1, 2, 3 )
 cycle cycle cycle ( => 1, 2, 3 ))
 
;; example #3
;; simple iteration
(xstap:
 ( _ (underscore) is convention for lambdas )
 { times dup 0 > if _ 1- times endif drop }

 ( this solution works, but we can mess with counter at stack )
 
 { _ "hello, world!" @one } 6 times
 { _ dup @one } 3 times)

;; example #4
;; loop to accumulate values
(xstap:
 ( previous loop can not be used to collect data into stack )
 ( but we can make another one, using our super-sequence )

 ( sequence consists of "foo", "bar" and "baz" )
 reset
 
 { iterate next "baz" = if else _ iterate endif }
 { _ dup 2 * }
 4 iterate @all ( => 4 8 16 ))

;; example #5
;; iterating over vector
(xstap:
 ( both items will be iteratable )
 { v [ 10 20 30 ] }
 { s "foxey" }

 { rotate [2 0 1] 3 shake }
 { spine-length swap len rotate }             ( drop counter )
 { each spine-length dup rotate < if _ 1+ each else drop endif }

 ( modify current index, set value of 97 + index )
 { _ dup rotate swap dup 97 + set swap @all }

 v 0 each
 s 1 each) ; => ("fbcde" [97 98 99])

;; `shake' can be used for many tasks

;; reorder the stack is the main purpose
(xstap: 1 2 3 4 "3210" 4 shake) ; => (1 2 3 4)
(xstap: 0 1 1 0 "1032" 4 shake) ; => (1 0 0 1)

;; format string can be empty, then
;; applying it will decrease stack elements count
(xstap: 4 5 6 "" 2 shake) ; => (4)

;; same index can appear in ordering vector multiple times
(xstap: 1 2 3 "0000" 1 shake) ; => (3 3 3 3 2 1)

;; one of the most useful new feature is temporal storage.
;; it can hold 1 element at any time, and that element
;; is last pop'ed thing.
;; we can take element from that storage infinite times
(xstap: 5 pop push push push) ; => (5 5 5)
(xstap: 777 pop 1 1 push 0 push) ; => (777 0 777 1 1)

;; this can be useful if you want to store element,
;; but without removing it from stack
(xstap: 6 store push) ; => (6 6)

(xstap:
 { func 1 2 3 }
 "func" @describe ( func: (1 2 3) )
 "concat" @describe ( word `concat' is not defined))

(xstap: [6 5] 1 nth swap drop) ; => (5)
(xstap: "speed" 0 nth 1 str) ; => ("s" "speed")

(xstap: [0 1 2] len) ; => (3 [0 1 2])
(xstap: "measure me!" len) ; => (11 "measure me")

(xstap: [0 1 2] split drop) ; => (1 2)
(xstap: "qwe" split) ; => (113 119 101)

;; `vec' and `str' are highly versatile:

;; create sequence from stack elements
(xstap: 0 1 2 3 vec) ; => ([2 1 0])
(xstap: ?0 ?1 ?2 3 str) ; => ("210")

;; create sequence from sequence of another type:
(xstap: "abc" vec) ; => ([97 98 99])
(xstap: [48 49 50] str) ; => ("012")

;; create empty sequences with n-capacity:
(xstap: -4 vec) ; => ([0 0 0 0])
(xstap: -4 str) ; => ("^@^@^@^@")

;; other operations on sequences:

(xstap: [0 0] 0 "foo" set 1 "bar" set) ; => (["foo" "bar"])
(xstap: "Aa" 0 ?a set 1 ?A set) ; => ("aA")

(xstap: [[8]] 0 nth 0 nth) ; => (8 [8] [[8]])

;;; extension from xe4 wordset:

(xstap:import-operators-dict)

(xstap: 0 0= 2 0=) ; => (0 -1)
(xstap: 2 0> -1 0>) ; => (0 -1)
(xstap: -1 0< 2 0<) ; => (0 -1)

(xstap: 7 2+ 9 2-) ; => (7 9)

(xstap: 5 make-vec) ; => ([0 0 0 0 0])
(xstap: 2 make-str) ; => ("^@^@")

(xstap: 0 1 pair) ; => ([1 0])
(xstap: 0 1 pair 1st swap 2nd) ; => (0 [1 0] 1)

;; with the introduction of temporal storage,
;; implementing some iteration functions using recursion
;; is now pretty trivial.
;; just keep in mind that too much recursion for now
;; is going to hit perfomance *REALLY HARD*!

;; all standard loops are iterating in reversed order.
;; there is a way of implementing forward iterations,
;; exmple will be shown after this section.
(xstap:
 ( `loop' is general looping from essential dictionary )
 { _ @one } 3 loop

 ( map over vector squaring its elements )
 [0 1 2 3] { _ dup sqr set } 4 loop ( => [0 1 4 9] )

 ( iterate, creating a range, then aggregate them in string )
 { n 4 } { _ ?a + } n loop n str ( => "abcd" )
 
 ( `times' is the simplest form of iteration )
 ( it repeats given `_' lambda n-times, giving you no current index )
 { _ "i am looping!" @one } 5 times

 ( every loop can be cancelled using `break' function )
 { _ 3 = if break else "loop: 4 cycles" @one endif } 8 loop
 { _ "times: 1 cycle" @one break } 4 times

 ( the easier way to iterate over an sequence is `walk' function )
 [0 0 0] { _ 10 set } walk ( => [10 10 10] )

 ( `reduce' and `map' are coming soon ))

;; creating forward-iterating primitive.
;; there are multiple ways of writing it,
;; here is the most trivial (with pair stashed instead of number)
(xstap:
 { for
   { break [0 0] pop } ( this hack makes our `for' breakable )
   dup pair pop ( 1st - current index, 2nd - iteration limit )
   { :iter-before push pair.rdiff swap 1st "02" 3 shake }
   { :iter-inside _ push pair.l-- pop } $iter drop
   ( if we want `break' work with default loops again, we restore it )
   { break 0 pop } }

 ( thats all we need to do )
   
 { _ 3 = if break else "3 times!" @one endif } 8 for
 { _ @one } 4 for)





