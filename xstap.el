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
(xstap:set-options '(return-stack-after-eval . nil)
		   '(flush-stack-before-eval . nil))

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
	  `((e4: ',words))
	  (when (gethash 'return-stack-after-eval xstap:options)
	    '(stap-stack))))

;;;; [ LANGUAGE EXTENSIONS ] ;;;;

;; there is small probability that some of those will
;; make their way into the predefined (builtin) STAP words,
;; mostly because of perfomance.
(defun xstap:import-extra-wordset ()
  "imports many useful words into your STAP dictionary (beware of name clashes)"
  (e4: '(( comparing with nil )
	   { 0= 0 = }
	   { 0< 0 < }
	   { 0> 0 > }
	 ( stack top manipulations )
	   { 2+ 2 + }
	   { 2- 2 - }
	 ( sequence constructors )
	   { MAKE-VEC NEG VEC }
	   { MAKE-STR NEG STR }
	   { PAIR 2 VEC }
	 ( sequence helpers )
	   { 1ST 0 NTH }
	   { 2ND 1 NTH }
	   { 3RD 2 NTH })))

