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

(defun xstap:import-stack-dict ()
  "imports stack utils"
  (stap: '({ reorder ( s1 -- )
	     len shake }
	     
	   { swap ( a1 a2 -- a2 a1 )
	     "10" reorder }
	   
	   { dup ( a1 -- a1 a1 )
	     "00" 1 shake }
	   
	   { drop ( a1 -- )
	     "" 1 shake }
	   
	   { ndrop ( n1 -- )
	     "" swap shake }
	   
	   { drop-all ( -- )
	     count ndrop })))

(defun xstap:import-sequence-dict ()
  "imports string and vector utils"
  (stap: '({ 1st ( [a1 ..] -- [a1 ..] a1 )
	     0 nth }
	     
	   { 2nd ( [a1 a2 ..] -- [a1 a2 ..] a2 )
	     1 nth }
	   
	   { 3rd ( [a1 a2 a3 ..] -- [a1 a2 a3 ..] a3 )
	     2 nth })))

(defun xstap:import-pairs-dict ()
  "imports functions useful when dealing with [x x] vectors;
REQUIRES: `stack', `sequence'"
  (stap: '({ pair ( a1 a2 -- [a2 a1] )
	     2 vec }
	     
	   { pair.lset ( [a1 a2] a3 -- [a3 a2] )
	     0 swap set }
	   
	   { pair.rset ( [a1 a2] a3 -- [a1 a3] )
	     1 swap set }
	     
	   { pair.l++ ( [n1 a1] -- [n2 a1] )
	     1st 1+ pair.lset }
	     
	   { pair.l-- ( [n1 a1] -- [n2 a1] )
	     1st 1- pair.lset }
	   
	   { pair.r++ ( [a1 n1] -- [a1 n2] )
	     2nd 1+ pair.rset }
	   
	   { pair.r-- ( [a1 n1] -- [a1 n2] )
	     2nd 1- pair.rset }
	   
	   { pair.vals ( [a1 a2] -- [a1 a2] a2 a1 )
	     1st swap 2nd "021" reorder }
	     
	   { pair.ldiff ( [n1 n2] -- n3 )
	     pair.vals - }
	     
	   { pair.rdiff ( [n1 n2] -- n3 )
	     pair.vals swap - })))
	   
(defun xstap:import-essential-dict ()
  "imports essential stdlib (name clashes with your definitions are possible)"
  (stap: ' ( iteration primitives )

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
  "includes `sqr', `abd' and others;
REQUIRES: `stack'"
  (stap: '({ sqr dup * }
	   { abs dup 0 < if neg endif }))) 
