(xstap:
 { rot "201" 3 shake }
 { drop "" 1 shake }
 { :size stash 0 nth }
 { .size :size swap drop }
 { .push 0 :size 1+ "021" 3 shake set 0 nth rot set drop }
 { .pop :size 0 > if 0 nth 1- 0 swap set else endif }
 { .pop :size 0 > if 0 nth 0 set 0 nth 1- 0 swap set endif }
 { .pop
   :size 0 > if
     0 nth nth
     swap
     0 nth 0 set
     0 nth 1- 0 swap set
   endif drop }
 1 .push 2 .push 3 .push .pop .pop .pop
)
4 0 [0 0] 0 "201"
