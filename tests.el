;; examples of E4 usage (see README also)

(xe4:set-options '(return-stack-after-eval . t)
		 '(flush-stack-before-eval . t))

;; immutable sequence 
(xe4:
 ( define sequence of "foo" "bar" "baz" )
 { sequence-reset
   { sequence-next
     "foo"
     { sequence-next
       "bar"
       { sequence-next
         "baz" } } } }
 { sequence-next sequence-reset }
       
 ( prints "for", "bar", "baz" )
 sequence-reset
 sequence-next ..
 sequence-next ..
 sequence-next ..
 sequence-reset
 ( and we can now use sequence-next again ))

;; self-restoring sequence
(xe4:
 { init { cycle head } }
 { head 1
 { cycle 2
 { cycle 3 init } } }
 
 init 
 cycle cycle cycle ( => 1, 2, 3 )
 cycle cycle cycle ( => 1, 2, 3 ))
 
