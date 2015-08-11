;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `XSTAP' is Extras for STAck Processing language ;;;;;

;; this is addition to `stap' package so it requires
;; loaded Emacs Forth before inclusion
;;
;; it can be considered as public API for the language,
;; one of possible implementations to use lower level
;; implementation (found in `stap.el')

;;;; [ OPTIONS ] ;;;;

(defun xstap:set-option (option)
  (puthash (car option) (cdr option) xstap:options))

(defun xstap:set-options (&rest options)
  (dolist (option options)
    (xstap:set-option option)))

(setq xstap:options (make-hash-table :test 'eq))

;; set default options
(xstap:set-options '(return-stack-after-eval . t)
		   '(flush-stack-before-eval . t))

;;;; [ ENVIRONMENT CONTROL ] ;;;;

(defun xstap:stack-flush ()
  "make STAP stack empty"
  (setq stap-stack '()))

(defmacro xstap:import (names)
  "import multiple dictionaries"
  (let ((sexp '(progn)))
    (dolist (name names)
      (push (list (intern-soft (concat "xstap:import-"
				       (symbol-name name)
				       "-dict")))
	    sexp))
    (reverse sexp)))

;;;; [ EXECUTION ] ;;;;

(defmacro xstap:with-stack-rollback (&rest forms)
  "save stack state, evaluate forms, then restore stack state"
  (let ((stack-state (make-symbol "stack-backup")))
    (let ((stack-state stap-stack))
      `(progn ,@forms)
      `(setq stap-stack ',stack-state))))

(defmacro xstap: (&rest words)
  "take some STAP words, evaluate them (with respect to `xstap:options')"
  (append '(progn)
	  (when (gethash 'flush-stack-before-eval xstap:options)
	    '((xstap:stack-flush)))
	  `((stap: ',words))
	  (when (gethash 'return-stack-after-eval xstap:options)
	    '(stap-stack))))

;;;; [ LANGUAGE EXTENSIONS ] ;;;;

(defun xstap:import-essential-dict ()
  "imports essential stdlib (name clashes with your definitions are possible)"
  (stap: '(( stack manipulations )
	   
	     { swap "10" 2 shake }
	     { dup "00" 1 shake }
	     
	     { drop "" 1 shake }
	     { ndrop "" swap shake }
	     { drop-all count ndrop }

	   ( stash utils )
	   
	     { store pop push }
	     { ++ push 1+ pop }
	     { -- push 1- pop }

	   ( sequence utils )
	     
	     { 1st 0 nth }
	     { 2nd 1 nth }
	     { 3rd 2 nth }
	   
	   ( pairs )

	     { pair 2 vec } 
	     { pair.lset 0 swap set }
	     { pair.rset 1 swap set }
	     { pair.l++ 1st 1+ pair.lset }
	     { pair.l-- 1st 1- pair.lset }
	     { pair.r++ 2nd 1+ pair.rset }
	     { pair.r-- 2nd 1- pair.rset }	     
	     { pair.vals 1st swap 2nd "021" 3 shake }
	     { pair.ldiff pair.vals - }
	     { pair.rdiff pair.vals swap - }

	   ( printing words )
	     
	     { @store push @one }
	   
	   ( iteration primitives )

	     { break 0 pop }

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

	     { $do/stack dup
	       0 = if drop else :do/stack $do/stack endif }
	     
	     { reduce len
	       { :do/stack 1- "010" 2 shake nth &loop-body swap } $do/stack })))

(defun xstap:import-math-dict ()
  "includes `sqr', `abd' and others requires `essential-dict'"
  (stap: '(( general functions )
	     { sqr dup * }
	     { abs dup 0 < if neg endif })))
 
(defun xstap:import-operators-dict ()
  "imports operators like `2+', `0=', etc."
  (stap: '(( comparing with nil )
	     { 0= 0 = }
	     { 0< 0 < }
	     { 0> 0 > }
	   ( stack manipulations )
	     { 2+ 2 + }
	     { 2- 2 - })))
