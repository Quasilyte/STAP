;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `e4' is Emacs Forth embedded interpreter ;;;;;

;; package naming convention:
;; e4:name -- exported (public) binding
;; e4.name -- unexported (private) binding
;;
;; e.g. `e4:' is exported, `e4.stack' is not 

;; word -- anything in `e4.dictionary'
;; tag -- something look like word, but defined at implementation level
;;
;; e.g. `{' and `}' are tags, but `IF' is a word

;;;; [ DATA SECTION ] ;;;;

(defconst e4.TRUE -1 "Forth usually returns -1 as truth value")
(defconst e4.FALSE 0 "only zero is considered as falsy value")

;; E4 environment variables
(setq e4.stack '() ; main data stack for execution
      e4.tokens '() ; usually this is an input data for interpreter
      e4.new-word '() ; car is word-symbol, cdr is its body
      e4.dict (make-hash-table :test 'equal) ; all defined words
      e4.eval-mode :interpret ;; can be `:interpret' or `:compile'
      e4.nest-level 0) ;; counter for nested definitions to find corresponding }

;;;; [ CORE ] ;;;;

;;; [ MISCELLANEOUS ]
;;; something not so tightly related to the E4 or its components

(defmacro e4.do-nothing ()
  "literally, do nothing at all. used for readability"
  '(lambda ()))

;;; [ STACK ]
;;; operations defined on `stack' (also known as data or parameter stack)

(defun e4.stack-drop ()
  "drop top element from the stack"
  (setq e4.stack (cdr e4.stack)))

(defun e4.stack-ndrop (n)
  "drop n top elements from the stack"
  (setq e4.stack (nthcdr n e4.stack)))

(defun e4.stack-pop ()
  "take top element from the stack"
  (pop e4.stack))

(defun e4.stack-npop (n)
  "take n top elements from the stack and return them as a list"
  (prog1
      (subseq e4.stack 0 n)
    (e4.stack-ndrop n)))

(defun e4.stack-push (scalar)
  "push scalar on top of the stack"
  (push (pcase scalar
	  (`t e4.TRUE) 
	  (`nil e4.FALSE)
	  (_ scalar))
	e4.stack))

;;; [ DICTIONARY ]
;;; operations defined on `dictionary'

(defun e4.dict-store (word fn)
  "insert or replace dictionary entry" 
  (puthash word fn e4.dict))

(defun e4.dict-fetch (word)
  "return dictionary entry, if it is stored (nil otherwise)"
  (gethash word e4.dict))

;;; [ WORDS ]
;;; operations defined on `words'

(defun e4.word-exec (word)
  "invoke word associated lambda"
  (let ((fn (e4.dict-fetch word)))
    (if fn 
	(if (functionp fn)
	    (e4.next-token-do (funcall fn)) ; predefined word
	  (setq e4.tokens (append fn (cdr e4.tokens))))
      (error (format "undefined e4 word: `%s'" word)))))

(defun e4.call-with-arity (fn arity)
  "call lisp function with args passed from the e4 stack"
  (apply fn (nreverse (e4.stack-npop arity))))

(defmacro e4.overloaded-op-lambda (type-switch arity)
  "create lambda calling lisp function returned from `type-switch'"
  `(lambda ()
     (let ((top (car e4.stack)))
       (e4.stack-push (e4.call-with-arity ,type-switch 2)))))

(defmacro e4.n-aggregation-lambda (n-cond)
  `(lambda ()
     (let ((n (e4.stack-pop)))
       (e4.stack-push ,n-cond))))

(defmacro e4.stack-reorder-lambda (n order)
  (let ((pops (make-list n '(e4.stack-pop)))
	(order (mapcar (lambda (i)
			 (list 'nth i 'elements)) order)))
    `(lambda ()
       (let ((elements (list ,@pops)))
	 (setq e4.stack (append (list ,@order) e4.stack))))))

;;; [ TOKENS ]
;;; operations defined on `tokens'

(defun e4.next-token ()
  "throw away current token and take another"
  (setq e4.tokens (cdr e4.tokens)))

(defmacro e4.next-token-do (action)
  "select next token, perform `action'. wrapped in `progn'"
  `(progn
     (e4.next-token)
     ,action))

(defmacro e4.while-token (&rest forms)
  "evaluate passed forms through tokens (token binding passed implicitly)"
  `(while e4.tokens
     (let ((token (car e4.tokens)))
       ,@forms)))

(defmacro e4.skip-tokens-until (termination-p)
  "skip current token and keep skipping until `termination-p' is t"
  `(catch 'break
     (e4.while-token
      (e4.next-token)
      (when ,termination-p (throw 'break nil)))))

;;; [ INTERPRET ]
;;; walking through tokens while evaluating them

(defun e4.finish-words-interpretation ()
  "exit interpetation mode and begin new word interning"
  (e4.next-token)
  (setq e4.eval-mode :compile))

(defun e4.take-scalar (scalar)
  "while moving to next token, store given scalar in stack"
  (e4.next-token)
  (e4.stack-push scalar))

(defun e4.interpreting-eval (word)
  "process word while in interpreter mode"
  (if (eq '{ word) ; mode switching symbol
      (e4.finish-words-interpretation)
    (if (symbolp word)
	(e4.word-exec word) 
      (e4.take-scalar word)))) ; everything except symbols considered scalars

;;; [ COMPILE ]
;;; walking through tokens while collecting them without evaluation

(defmacro e4.nest-level-modify (mutator)
  "change `nest-level' value by applying mutator function (usually 1+ or 1-)"
  `(setq e4.nest-level (,mutator e4.nest-level)))

(defun e4.new-word-set (word)
  "put user-defined word in dictionary"
  (setq e4.new-word (cons word nil)))

(defun e4.compilation-closing-tag ()
  "handle compilation terminating tag (it should be balanced with open tag)"
  (cond ((= 0 e4.nest-level) (e4.finish-word-compilation))
	(t (e4.nest-level-modify 1-) ; nested word definition end
	   (push word e4.new-word))))

(defun e4.finish-word-compilation ()
  "finalize word compilation and enter interpretation mode"
  (let ((new-word (nreverse e4.new-word)))
    (e4.dict-store (car new-word) (cdr new-word)))
  (setq e4.eval-mode :interpret
	e4.new-word '()))

(defun e4.compiling-eval (word)
  "process word while in compiler mode"
  (if (eq '} word) ; mode switching symbol
      (e4.compilation-closing-tag)
    (cond (e4.new-word ; if new name is already registered
	   (when (eq '{ word) (e4.nest-level-modify 1+)) ; nested word definition ?
	   (push word e4.new-word))
	  (t (e4.new-word-set word)))) ; otherwise, store it
  (e4.next-token))

;;; [ EVALUATION ]
;;; the highest level, entry point

(defun e4.reload-environment (tokens)
  "prepare E4 environment to run expressions"
  (setq e4.eval-mode :interpret) ; we must always start from this mode
  (setq e4.tokens tokens))

(defun e4: (words)
  "take some E4 words as a list, evaluate them"
  (e4.reload-environment words)
  (e4.while-token
   (if (listp token)
       (e4.next-token) ; if it is a list, then it is a comment
     (if (eq :interpret e4.eval-mode) ; else process token in current mode
	 (e4.interpreting-eval token)
       (e4.compiling-eval token)))))

;;;; [ BUILTINS ] ;;;;

;;; [ PREDEFINED: FUNDAMENTAL ]
;;; arithmetics, basic and generic operators

;; (word binded-lisp-fn arity)
(dolist (binding '((- 2)
		   (/ 2)
		   (* 2)
		   (< 2)
		   (> 2)
		   (1+ 1)
		   (1- 1)
		   (NEG - 1)))
  (let* ((word (car binding))
	(fn (if (= 2 (length binding)) word (cadr binding)))
	(arity (car (last binding))))
    (e4.dict-store
     word `(lambda ()
	     (e4.stack-push (e4.call-with-arity ',fn ,arity))))))

;; only 0 number is considered false (can be changed later)
(e4.dict-store
 '! (lambda ()
      (let ((top (e4.stack-pop)))
	(e4.stack-push (if (and (numberp top) (zerop top))
			   e4.TRUE
			 e4.FALSE)))))

(e4.dict-store
 '+ (e4.overloaded-op-lambda (cond ((numberp top) '+)
				   ((stringp top) 'concat)) 2))

(e4.dict-store
 '= (e4.overloaded-op-lambda (cond ((numberp top) '=)
				   ((stringp top) 'string=)) 2))

;;; [ PREDEFINED: STACK ]
;;; common ways to manipulate parameter stack

(e4.dict-store
 'DROP (lambda () (e4.stack-pop)))

(e4.dict-store
 'NIP (lambda ()
	(let ((top (e4.stack-pop)))
	  (e4.stack-pop) ; drop the second element
	  (setq e4.stack (cons top e4.stack)))))

(e4.dict-store
 'DUP (lambda () (e4.stack-push (car e4.stack))))

(e4.dict-store
 'SWAP (lambda ()
	 (setq e4.stack (append (nreverse (list (e4.stack-pop)
		       			(e4.stack-pop)))
				e4.stack))))

(e4.dict-store
 'TUCK (e4.stack-reorder-lambda 2 (0 1 0)))

(e4.dict-store
 'OVER (e4.stack-reorder-lambda 2 (1 0 1)))

(e4.dict-store
 'ROT (e4.stack-reorder-lambda 3 (2 0 1)))

(e4.dict-store
 'DEPTH (lambda () (e4.stack-push (length e4.stack))))

;;; [ PREDEFINED: DISPLAY ]
;;; words useful for debugging and interactive development

(e4.dict-store
 '.. (lambda () (message "%s" (e4.stack-pop))))

(e4.dict-store
 '.s (lambda () (message "<%d> %s" (length e4.stack) e4.stack)))

(e4.dict-store
 'SEE (lambda ()
	(let* ((word (intern-soft (e4.stack-pop)))
	       (body (e4.dict-fetch word)))
	  (if body
	      (message "%s: %s" word body)
	    (message "word `%s' is not defined" word)))))

;;; [ PREDEFINED: CONTROL FLOW ]
;;; conditionals, loops (if any will ever appear, they should be here)

(e4.dict-store
 'IF (lambda ()
       (when (= e4.FALSE (e4.stack-pop))
	 (e4.skip-tokens-until (or (eq 'ENDIF token)
				   (eq 'ELSE token))))))

(e4.dict-store
 'ELSE (lambda ()
	 (e4.skip-tokens-until (or (eq 'ENDIF token)
				   (eq 'ELSE token)))))

(e4.dict-store
 'ENDIF (e4.do-nothing))

;;; [ PREDEFINED: SEQUENCE ]
;;; sequence operations (vectors & strings)

(e4.dict-store
 'NTH (lambda ()
	(let ((index (e4.stack-pop)))
	  (e4.stack-push (elt (car e4.stack) index)))))

(e4.dict-store
 'LEN (lambda ()
	(e4.stack-push (length (car e4.stack)))))

(e4.dict-store
 'SET (lambda ()
	(let ((value (e4.stack-pop)) (index (e4.stack-pop)))
	  (aset (car e4.stack) index value))))

(e4.dict-store
 'SPLIT (lambda ()
	  (setq e4.stack (append (e4.stack-pop) e4.stack))))

(e4.dict-store
 'VEC (e4.n-aggregation-lambda (cond ((stringp n) (vconcat n))
				      ((> n 0) (vconcat (e4.stack-npop n)))
				      ((< n 0) (make-vector (abs n) 0)))))

(e4.dict-store
 'STR (e4.n-aggregation-lambda (cond ((vectorp n) (concat n))
				      ((> n 0) (concat (e4.stack-npop n)))
				      ((< n 0) (make-string (abs n) 0)))))

;;;; [ FRIEND IMPORTS ] ;;;;

;; E4 without it is not very user-friendly
(load-file
 (expand-file-name "xe4.el" (file-name-directory (or load-file-name
						     buffer-file-name))))
