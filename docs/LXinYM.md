```elisp
;; STAP -- STAck Processing language 
;;
;; STAP is a high level [mostly] functional Forth dialect with a taste of lisp.
;; dynamically typed, containing only basic data structures and
;; generic operations on them, language is super slim and easy to learn.

;; this is embedded Emacs Lisp implementation for Stap, but
;; the latter is not bounded to it as we can freely make implementation
;; for any platform (thanks to Forth roots, Stap is nearly syntaxless).

;; github repository: https://github.com/Quasilyte/STAP

;; knowledge in Forth and/or Lisp will be handy to dive into STAP faster

;; first, we load the `stap.el' and setup the Emacs environment:
(xstap:set-options '(return-stack-after-eval . t)
		   '(flush-stack-before-eval . t))

;; get some words (functions) from standard library
(xstap:import (essential math))

;; all STAP code is written inside (xstap: ... ).
;; this whole invocation will return data stack as a list
;; into calling lisp code.
(xstap:
  ( now, this is not Lisp anymore )
 
"hello, world" @one ( works as expected )

  ( instead of variables, we use global parameter stack )
  (  using any literal in a code pushes it on top of the stack )

2     	      ( stack: <top [2]> )
4.5   	      ( stack: <top [4.5 2]> )
"str"         ( stack: <top ["str" 4.5 2]> )
[?a "xyz" []] ( stack: <top [[97 "xyz" []] "str" 4.5 2]> )

  ( everything except scalars is words (functions) )

@one ( prints `[97 "xyz" []]' and removes printed element )

  ( now we have 3 elements in stack, `count' word can tell that )

count ndrop ( cleans the stack completely )

  ( lets implement our version of ndrop )
  ( we introduce a new word by `{' name ...words `}',
    but first we need to learn `shake' word )

4 2          ( <top [2 4]> )
"10" 2 shake ( <top [4 2]>, this feel like swap, right? ) drop drop

  ( `shake' can be used to reorder some stack elements,
     to remove or duplicate them )

{ swap ( n1 n2 -- n2 n1 ) "10" 2 shake }
{ ndrop "" swap shake } ( our own ndrop, which redefines the one used earlier )
{ drop-all count ndrop } ( to clear the stack in 1 touch )
{ dup ( n1 -- n1 n1 )  "00" 1 shake }

[1 2] "dwarf" swap ( <top [[1 2] "dwarf"] )
2 ndrop ( <top []> )
4 dup ( <top [4 4]> ) drop-all

  ( word is "invoked" without any arguments, but somewhere in its body
    an operation which takes elements from stack can be performed )
  ( visually, its look like postfix application )

4.0 1.5 + ( <top [5.5]> )
"over" "lord" + ( <top ["overlord" 5.5]>, you got the idea )
9 neg ( => -9 )
9 1+  ( => 10 )
1 0 = ( => 0 )
1 1 = ( => -1 (everything except 0 is true) )
-1 !  ( => 0 ) drop-all

0 1 = ! if "true" endif                         ( => "true" ) drop
0 0 = if "true" else "false" endif              ( => "true" ) drop
0 if "one" else 0 0 = if "two" else "three" endif ( => "two" )   drop

{ fib ( n -- n ) 
  1- dup 1 > if dup fib swap 1- fib + endif }

10 fib ( => 34 ) drop

  ( it is possible to get defined function body )

"fib" @describe ( => (1- dup 1 > if dup fib swap 1- fib + endif) )

  ( language contains no variables, only functions )

{ my-var 777 } my-var @one ( => "777" )
{ my-var "function redefined" } my-var @one ( => "function redefined" )
{ var2 my-var }

  ( nested definitions are possible, they are evaluated only at invocation;
    it is also OK for a word to redefine itself )

{ var "magic" { var 777 } }
var var var ( => "magic", 777, 777 ) drop-all

  ( you see, we can you another helper function to "restore" initial
    word definition )
  (we are going to describe simple sequence now )

( sequence of "foo", "bar", "baz" )
{ reset
  { next "foo"
  { next "bar"
  { next "baz" } } } }
{ next reset }

reset                ( init sequence )
next next next       ( => "foo", "bar", "baz" )
reset next next next ( same as above )

  ( we can code "restoring" right into the definition )

{ init { cycle head } }
  { head 1
  { cycle 2
  { cycle 3 init } } }

init 
cycle cycle cycle ( => 1, 2, 3 )
cycle cycle cycle ( => 1, 2, 3 ) drop-all

  ( now we will cover another fundamental things before proceed
    with functions (there is a lot more tricks related to them) )

  ( we can split sequences into separate values )

[0 1 2] split ( <top [2 1 0]> )   
"abc" split   ( <top [99 98 97]> )

  ( of course we can also build up a sequence from values )

[0 1 2] split 3 vec ( => [0 1 2] )
?c ?b ?a 3 str      ( => "abc" )  

  ( vector can be casted to string and string can become a vector )

[?m ?o ?n ?k ?e ?y] str ( => "monkey" ) 
"monkey" vec            ( => [109 111 110 107 101 121])
 
  ( empty sequence created with negative length argument )

-5 vec ( => [0 0 0 0 0] )
-5 str ( => "\0\0\0\0\0" )

  ( to modify sequence n-th element, use `set' function )

"oracle" 0 ?O set        ( => "Oracle" )
[1 0] 1 ["<3 Forth"] set ( => [1 ["<3 Forth"]] )

  ( our stack is pretty messy right now, I was not calling drop for a while )

drop-all 

  ( no loops shown at this moment )
  ( well, there are none, but they can be
    expressed without big overhead (you can do full TCO here) )

{ do-times dup 0 > if &loop-body 1- do-times endif drop }

  ( this solution works, but we can mess with counter at stack )

{ &lambda "hello, world!" @one } 6 do-times ( prints a message 6 times )
{ &do dup @one } 3 do-times                 ( prints current counter value )

  ( do you noticed we used &name as something like lambda?
    all words starting with `&' share same memory cell, so every
    definition replaces it. &foo and &bar are equal symbols internally. )

{ &unnamed 1 2 3 }  ( I prefer call them "unnamed" rather than lambda )
"&" "lambda" rename ( no we gave a name to unnamed function )

lambda lambda ( => 3 2 1 3 2 1 )

  ( we can rename any word, and we can remove them with that )

"lambda" "&" rename ( now lambda is unnamed again )
 
  ( previous loop can not be used to collect data into stack
    but we can make another one, using our sequence )

 reset ( sequence consists of "foo", "bar" and "baz" )

{ seq-do next "baz" = if else _ seq-do endif }
{ _ dup 2 * }

4 seq-do ( => 4 8 16 ) drop-all

  ( next, we can iterate over real sequences )

drop-all

{ v [ 10 20 30 ] }
{ s "foxey" }

{ rotate "201" 3 shake }
{ spine-length swap len rotate }               ( drop counter )
{ each spine-length dup rotate < if && 1+ each else drop endif }

( modify current index, set value of 97 + index )
{ && dup rotate swap dup 97 + set swap @all }

v 0 each
s 1 each ("fbcde" [97 98 99])

drop-all

  ( still, this cannot be used in real programs,
    we need some convenient way to store counter
    out of the data stack )

5 pop ( takes top element and stores it at one cell stash )
push push push ( => 5, 5, 5 )

  ( with pop and push it is possible to build better loops )

{ break 0 pop } 

( stash-based loop prototype. *IMPORTANT* we can write it more effectively )
{ $do/stash -- push -1 > if :do/stash $do/stash endif }

{ times pop { :do/stash &loop-body } $do/stash }

{ loop pop
  { :do/stash push &loop-body } $do/stash }

{ index-walk len loop }

{ val-walk len pop
  { :do/stash push nth &loop-body } $do/stash }

{ map! len pop
  { :do/stash push nth &loop-body push swap set } $do/stash }

{ map copy map! }

  ( $ and : are not syntax tokens, they are parths of 
    word name, but they can improve readability.
    when making some *very* general idea, we end up
    in function, which is only partly implemented, 
    the other parts should be implemented by particular
    user. this reminds you about interfaces?
    I use $ to prefix such prototypes
    and : to mark "injected" functions )

( no iteration counter with `times' (fastest loop) )
{ &yield "yay!" } 3 times ( => "yay!" "yay!" "yay!" )

( `loop' puts iteration number at stack on each iteration )
{ &yield dup } 2 loop ( => 0 0 1 1 )

( `index-walk' goes through the sequence giving you the iteration number )
[7 7 7] { && nth swap } index-walk ( => [7 7 7] 7 7 7 )

( `val-walk' is like `index-walk', but shares the sequence values with you )
[7 7 7] { && swap } val-walk ( => [7 7 7] 7 7 7)

( `map!' iterates over sequence and changes its values )
( you get current value at each iteration and should return the replacement )
"foo" { && 1+ } map! ( => "gpp" )

( `map' is useful when you want to map, but do not want to 
  modify input sequence (it modifies the copy) )
"nom" { && 1- } map ( => "mnl", "nom" )

  ( remember *IMPORTANT* note above? we must optimize that 
    $do/stash, because now it consumes additional memory whenever
    it calls itself )

{ $do/stash-iter -- push 1- > if :do/stash $do/stash-iter }
{ $do/stash $do/stash-iter endif }

  ( we made another helper (enclosing) function, which contains `endif'
    removed from the `iter' body. this way, recursive call is
    really the last instruction in the body, so recursion becomes free )

  ( `reduce' can also be implemented (but my current implementation
    is pretty crude) )

{ $do/stack-iter dup 0 = if drop else :do/stack $do/stack-iter }
{ $do/stack $do/stack-iter endif }
{ reduce len
  { :do/stack 1- "010" 2 shake nth &loop-body swap } $do/stack }

  ( our function to scan element by predicate )

{ find dup push :pred if pop else drop endif }

                     ( initial min )
{ :pred < } [0 8 2 10 7] 1st pop { &with find } reduce
push @one ( => 0 )

                     ( initial max )
{ :pred > } [0 8 2 10 7] 1st pop { &with find } reduce
push @one ( => 10 ) 

( define function, make injectable hooks (:)
  composite code with generic prototypes ($), this is the STAP programming )
  
  ( showed above `rename' can be used to imitate namespaces )

{ hello "I greet the power users" @one }
( more library code ... )
"hello" "my-hello" rename ( somewhere in the end )

( now all users must use "my-hello" instead of "hello" )

drop-all

  ( you just learned a whole new language! good job!
    hope you enjoyed this article. )	
)

;; to check printed messages, visit *Messages* buffer.
```
