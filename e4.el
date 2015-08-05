;; package naming convention:
;; e4:name -- exported (public) binding
;; e4.name -- unexported (private) binding
;;
;; e.g. `e4:' is exported, `e4.stack' is not 

;;;; data section ;;;;

;; e4 environment variables
(setq e4.stack '() ; main data stack for execution
      e4.dictionary (make-hash-table :test 'equal) ; all defined words
      e4.eval-mode :interpret
      e4.new-word-symbol nil)

;;;; functions ;;;;

;;; stack operations

(defun e4.stack-pop ()
  "take top element from the stack"
  (pop e4.stack))

(defun e4.stack-push (scalar)
  "push scalar on top of the stack"
  (push (pcase scalar
	  (`t -1) ; true in forth is usually -1
	  (`nil 0) ; false in forth is usually 0
	  (_ scalar))
	e4.stack))

;;; word/dictionary manipulations

(defun e4.word-register (word fn)
  "extend or update word dictionary" 
  (puthash word fn e4.dictionary))

(defun e4.user-word-register (word)
  (setq e4.new-word-symbol word)
  (puthash word '() e4.dictionary))

(defun e4.word-exec (word)
  "invoke word associated lambda"
  (let ((fn (gethash word e4.dictionary)))
    (if fn
	(if (functionp fn)
	    (funcall fn)
	  (progn
	    (message "calling user func")
	    (message "fn: %s" fn)
	    (message "stack: %s" e4.stack)
	    (e4:from-list fn)))
	    ;(setq e4.stack (append (e4:from-list fn) e4.stack))))
	  ;(e4:from-list fn))
      (error (format "undefined e4 word: `%s'" word)))))

(defun e4.call-with-arity (fn arity)
  "call lisp function with args passed from the e4 stack"
  (let ((args (nreverse (subseq e4.stack 0 arity))))
    (setq e4.stack (subseq e4.stack arity))
    (apply fn args)))

;;; evaluation

(defun e4.interpreting-eval (word)
  "process word while in interpreter mode"
  (if (eq '{ word)
      (setq e4.eval-mode :compile)
    (funcall (if (symbolp word)
		 'e4.word-exec
	       'e4.stack-push)
	     word)))

(defun e4.compiling-eval (word)
  "process word while in compiler mode"
  (if (eq '} word)
      (e4.finish-word-compilation)
    (if e4.new-word-symbol
	(push word (gethash e4.new-word-symbol e4.dictionary))
      (e4.user-word-register word))))

(defun e4.finish-word-compilation ()
  (puthash e4.new-word-symbol
	   (nreverse (gethash e4.new-word-symbol e4.dictionary))
	   e4.dictionary)
  (setq e4.eval-mode :interpret
	e4.new-word-symbol nil))

;; e4 should be evaluated by users with this
(defmacro e4: (&rest words)
  "take some e4-forth words, evaluate them and return resulted stack"
  `(e4:from-list ',words))

;; this is mostly for internal usage, but can be useful for public API as well
(defun e4:from-list (words)
  "take some e4-forth words, evaluate them and return resulted stack"
  (setq e4.eval-mode :interpret)
  (dolist (word words)
    (when (not (listp word))
      (funcall (if (eq :interpret e4.eval-mode)
		   'e4.interpreting-eval
		 'e4.compiling-eval)
	       word)))
  e4.stack)

;;; advanced api

(defun e4:stack-flush ()
  (setq e4.stack '()))

(defmacro e4:with-stack-rollback (&rest forms)
  "save stack state, evaluate forms, then restore stack state"
  (let ((stack-state (make-symbol "stack-backup")))
    (let ((stack-state e4.stack))
      `(progn ,@forms)
      `(setq e4.stack ',stack-state))))

(defmacro e4:with-empty-stack (&rest forms)
  "flush the stack and evaluate given forms"
  (e4:stack-flush)
  `(progn ,@forms))

;;; predefined e4 words (incomplete FORTH-83 standart)

;; basic binary and unary operators
(dolist (pair '((+ 2) (- 2) (/ 2) (* 2) (= 2) (< 2) (> 2)
		(1+ 1) (1- 1)))
  (let ((word (car pair)) (arity (nth 1 pair)))
    (e4.word-register
     word `(lambda ()
	    (e4.stack-push (e4.call-with-arity ',word ',arity))))))

