;; examples of E4 usage (see README also)

(xe4:set-options '(return-stack-after-eval . t)
		 '(flush-stack-before-eval . t))

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
 
