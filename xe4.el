;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `xe4' is Extras for Emacs Forth ;;;;;

;; this is addition to `e4' package so it requires
;; loaded Emacs Forth before inclusion

;;;; options ;;;;

(defun xe4:set-option (option)
  (puthash (car option) (cdr option) xe4:options))

(defun xe4:set-options (&rest options)
  (dolist (option options)
    (xe4:set-option option)))

(setq xe4:options (make-hash-table :test 'eq))

;; set default options
(xe4:set-options '(return-stack-after-eval . nil)
		 '(flush-stack-before-eval . nil))

;;;; control functions ;;;;

(defun xe4:stack-flush ()
  "make E4 stack empty"
  (setq e4.stack '()))

;;;; execution functions ;;;;

(defmacro xe4:with-stack-rollback (&rest forms)
  "save stack state, evaluate forms, then restore stack state"
  (let ((stack-state (make-symbol "stack-backup")))
    (let ((stack-state e4.stack))
      `(progn ,@forms)
      `(setq e4.stack ',stack-state))))

(defmacro xe4: (&rest words)
  "take some E4 words, evaluate them (with respect to `xe4:options')"
  (append '(progn)
	  (when (gethash 'flush-stack-before-eval xe4:options)
	    '((xe4:stack-flush)))
	  `((e4: ',words))
	  (when (gethash 'return-stack-after-eval xe4:options)
	    '(e4.stack))))
	  
;;;; e4 extension ;;;;

;; there is small probability that some of those will
;; make their way into the predefined (builtin) E4 words,
;; mostly because of perfomance.
(defun xe4:import-extra-wordset ()
  "imports many useful words into your E4 dictionary (beware of name clashes)"
  (e4: '(( comparing with nil )
	 { 0= 0 = }
	 { 0< 0 < }
	 { 0> 0 > }
	 ( stack top manipulations )
	 { 2+ 2 + }
	 { 2- 2 - }
	 ( some other helpers )
	 { MAKE-VEC NEG VEC }
	 { MAKE-STR NEG STR })))

	     
	 
	  
	   
	 
