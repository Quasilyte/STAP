;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `XSTAP' is Extras for STAck Processing language ;;;;;

;; this is addition to `e4' package so it requires
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
  "imports most useful definitions (like `swap' and `drop')"
  (stap: '(( stack manipulations )
	   { swap "10" 2 shake }
	   { drop "" 1 shake }
	   { ndrop "" swap shake }
	   { dup "00" 1 shake } 
	   { store pop push })))

(defun xstap:import-extra-dict ()
  "imports many useful words into your STAP dictionary (beware of name clashes)"
  (stap: '(( comparing with nil )
	     { 0= 0 = }
	     { 0< 0 < }
	     { 0> 0 > }
	   ( stack top manipulations )
	     { 2+ 2 + }
	     { 2- 2 - }
	   ( sequence constructors )
	     { make-vec neg vec }
	     { make-str neg str }
	     { pair 2 vec }
	   ( sequence helpers )
	     { 1st 0 nth }
	     { 2nd 1 nth }
	     { 3rd 2 nth })))
