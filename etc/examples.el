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
 { n-times dup 0 > if &loop-body 1- n-times endif drop }

 ( this solution works, but we can mess with counter at stack )
 
 { &do "hello, world!" @one } 6 n-times
 { &do dup @one } 3 n-times)

;; example #4
;; loop to accumulate values
(xstap:
 ( previous loop can not be used to collect data into stack )
 ( but we can make another one, using our super-sequence )

 ( sequence consists of "foo", "bar" and "baz" )
 reset
 
 { iterate next "baz" = if else && iterate endif }
 { &apply dup 2 * }
 4 iterate @all ( => 4 8 16 ))

;; example #5
;; iterating over vector
(xstap:
 ( both items will be iteratable )
 { v [ 10 20 30 ] }
 { s "foxey" }

 { rotate [2 0 1] 3 shake }
 { spine-length swap len rotate }             ( drop counter )
 { each spine-length dup rotate < if && 1+ each else drop endif }

 ( modify current index, set value of 97 + index )
 { && dup rotate swap dup 97 + set swap @all }

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

;; no iteration counter, simple repetitons (fastest loop)
(xstap:
 { && "yay!" } 3 times) ; => ("yay!" "yay!" "yay!")

;; `loop' puts iteration number at stack on each iteration
(xstap:
 { && dup } 2 loop) ; => (0 0 1 1)

;; `index-walk' goes through the sequence giving you the iteration number.
;; equal to "len loop" call
(xstap:
 [7 7 7] { && nth swap } index-walk) ; => ([7 7 7] 7 7 7)

;; `val-walk' is like `index-walk', but shares the sequence values with you
(xstap:
 [7 7 7] { && swap } val-walk) ; => ([7 7 7] 7 7 7)

;; `map!' iterates over sequence and changes its values;
;; you get current value at each iteration and should return the replacement
(xstap:
 "foo" { && 1+ } map!) ; => ("gpp")

;; map is useful when you want to map, but do not want to modify
;; input sequence (it modifies the copy)
(xstap:
 "nom" { && 1- } map) ; => ("mnl" "nom")

;; reduce is a special loop. it does not use stash, but stores
;; iteration information on stack (so be careful).
;; anyway, it is extremely useful in some cases
(xstap:
 { find dup push :pred if pop else drop endif }

 ( find min )
 { :pred < } [0 8 2 10 7] 1st pop { && find } reduce push
 swap drop ( delete forst vec )

 ( find max )
 { :pred > } [0 8 2 10 7] 1st pop { && find } reduce push
 swap drop ( delete second vec )) ; => (10 0)
