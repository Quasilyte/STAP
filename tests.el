(xstap:import (essential math))

(equal '(1 2 3) '(1 2 4))

(defmacro test (:expect result :name name &rest forms)
  (setq stap-stack '())
  (stap: forms)
  (when (not (equal result stap-stack))
    (error "`%s' test failed! %s vs %s" name result stap-stack)))

(test
 :expect ([1 1 1] "one" 1) :name "stack"
 1 "one" [1 1 1])

(test
 :expect (2 1) :name "ignore comments"
 1 ( my name is comment )
 2 ( "comments can" [ contain ] ( anything ) 'inside )
 ())

(test
 :expect (-9 8 10 9 5 4) :name "num-arithm"
 8 4 -
 10 2 /
 3 3 *
 9 1+
 9 1-
 9 neg)

(test
 :expect (-1 -1 -1 "foobarbaz" 0 0 4) :name "overloaded & boolean op"
 1 3 +
 1 1 = !
 1 0 =
 "foo" "bar" "baz" + +
 "foo" "foo" =
 "foo" "bar" = !
 "" "" =)

(test
 :expect (1 2 3) :name "shake-rotate"
 1 2 3 "210" 3 shake)

(test
 :expect (2 2 2 1) :name "shake-duplicate"
 1 2 "000" 1 shake)

(test
 :expect (1) :name "shake-drop"
 1 2 3 "" 2 shake)

(test
 :expect (3 3 2 1) :name "count"
 1 2 3 count)

(test
 :expect (["x"] 50 75 100) :name "stash"
 100 50 pop
 75 push
 ["x"] pop
 push pop push)

(test
 :expect (3 2 1 "ok" "ok" "ok") :name "conditionals"
 -1 if "ok" else ":(" endif
 0 if ":(" else "ok" endif
 0 if ":(" else -1 if "ok" endif
 0 if ":(" else 0 if ":(" else endif
 9 9 = if 1 2 3 endif)

(test
 :expect (?y "xyz" 8 [8 4 2]) :name "nth"
 [8 4 2] 0 nth
 "xyz" 1 nth)

(test
 :expect (0 "" 3 "3rd" 0 [] 3 [8 4 2]) :name "len"
 [8 4 2] len
 [] len
 "3rd" len
 "" len)

(test
 :expect ("xyO" ["value" 4 2]) :name "set"
 [8 4 2] 0 "value" set
 "xyz" 2 ?O set)

(test
 :expect (?x ?y ?z 8 4 2) :name "split"
 [] split
 [8 4 2] split
 "" split
 "xyz" split)

(test
 :expect ([?q ?w ?e ?r] [0 0] [8 4 2]) :name "vec"
 2 4 8 3 vec
 -2 vec
 "qwer" vec)

(test
 :expect ("aA" "\x0\x0" "hello") :name "str"
 ?o ?l ?l ?e ?h 5 str
 -2 str
 [?a ?A] str)

(test
 :expect ("copy me" "copy me" "" "" [5 "oh" 5] [5 "oh" 5] [] []) :name "copy"
 [] copy
 [5 "oh" 5] copy
 "" copy
 "copy me" copy)

(test
 :expect ("give me a name!" "shiftedshifted" "@@" "##" "##") :name "rename"
 { defun "#" "#" + "defun" "&" rename }
 defun &unnamed
 { shifter dup "021" 3 shake rename dup + }
 "shifter" "@" shifter
 "@" "shifted" @
 { &unnamed "give me a name!" }
 "&" "named" rename
 named)

(test
 :expect ("bark!" "bark!") :name "definition"
 { dog-says "bark!" "bark!" }
 dog-says drop drop
 dog-says)

(test
 :expect (2 1 0 2 1 0) :name "nested definition"
 { reset
   { iter 0
   { iter 1
   { iter 2 } } } }
 reset
 iter iter iter reset
 iter iter iter)

(test
 :expect (3 2 1) :name "unnamed definitions"
 { apply &unnamed }
 { && 1 } apply
 { &lambda 2 } apply
 { & 3 } apply)

(test
 :expect (6 14) :name "definition composition"
 { $generic 4 :operation }
 { 4+ { :operation + } $generic }
 { 4- { :operation - } $generic }
 10 4+
 10 4-)

(message "everything passed, everything seems to be OK at this time")




