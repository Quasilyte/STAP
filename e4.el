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

;;;; data section ;;;;

(defconst e4.TRUE -1 "Forth usually returns -1 as truth value")
(defconst e4.FALSE 0 "only zero is considered as falsy value")

;; e4 environment variables
(setq e4.stack '() ; main data stack for execution
      e4.tokens '() ; usually this is an input data for interpreter
      e4.new-word '() ; car is word-symbol, cdr is its body
      e4.dictionary (make-hash-table :test 'equal) ; all defined words
      e4.eval-mode :interpret
      e4.nest-level 0)

;;;; functions ;;;;

;;; stack operations

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

;;; word/dictionary manipulations

(defun e4.word-register (word fn)
  "extend or update word dictionary" 
  (puthash word fn e4.dictionary))

(defun e4.word-exec (word)
  "invoke word associated lambda"
  (let ((fn (gethash word e4.dictionary)))
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

;;; utils

(defmacro e4.do-nothing ()
  "literally, do nothing at all"
  '(lambda ()))

(defmacro e4.nest-level-modify (mutator)
  "change `nesl-level' value bu applying mutator function (usually 1+ or 1-)"
  `(setq e4.nest-level (,mutator e4.nest-level)))

(defun e4.next-token ()
  "throw away current token and take another"
  (setq e4.tokens (cdr e4.tokens)))

(defmacro e4.next-token-do (action)
  "select next token, perform `action'"
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

;;; interpret-related

(defun e4.interpreting-eval (word)
  "process word while in interpreter mode"
  (if (eq '{ word) ; mode switching symbol
      (e4.next-token-do (setq e4.eval-mode :compile))
    (if (symbolp word)
	(e4.word-exec word)
      (e4.next-token-do (e4.stack-push word)))))

;;; compile-related

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
    (puthash (car new-word) (cdr new-word) e4.dictionary))
  (setq e4.eval-mode :interpret
	e4.new-word '()))

(defun e4.compiling-eval (word)
  "process word while in compiler mode"
  (if (eq '} word) 
      (e4.compilation-closing-tag)
    (cond (e4.new-word
	   (when (eq '{ word) (e4.nest-level-modify 1+)) ; nested word definition ?
	   (push word e4.new-word))
	  (t (e4.new-word-set word))))
  (e4.next-token))

;;; evaluation

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
     (funcall (if (eq :interpret e4.eval-mode)
		  'e4.interpreting-eval
		'e4.compiling-eval)
	      token))))

;;;; predefined e4 words ;;;;

;;; fundamentals

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
    (e4.word-register
     word `(lambda ()
	     (e4.stack-push (e4.call-with-arity ',fn ,arity))))))

;; only 0 number is considered false (can be changed later)
(e4.word-register
 '! (lambda ()
      (let ((top (e4.stack-pop)))
	(e4.stack-push (if (and (numberp top) (zerop top))
			   e4.TRUE
			 e4.FALSE)))))

(e4.word-register
 '+ (e4.overloaded-op-lambda (cond ((numberp top) '+)
				   ((stringp top) 'concat)) 2))

(e4.word-register
 '= (e4.overloaded-op-lambda (cond ((numberp top) '=)
				   ((stringp top) 'string=)) 2))

;;; data stack manipulators

(e4.word-register
 'DROP (lambda () (e4.stack-pop)))

;; it should be faster than `e4.stack-reorder-lambda',
;; but benchmarks are needed.
(e4.word-register
 'NIP (lambda ()
	(let ((top (e4.stack-pop)))
	  (e4.stack-pop) ; drop the second element
	  (setq e4.stack (cons top e4.stack)))))

(e4.word-register
 'DUP (lambda () (e4.stack-push (car e4.stack))))

(e4.word-register
 'SWAP (lambda ()
	 (setq e4.stack (append (nreverse (list (e4.stack-pop)
		       			(e4.stack-pop)))
				e4.stack))))

(e4.word-register
 'TUCK (e4.stack-reorder-lambda 2 (0 1 0)))

(e4.word-register
 'OVER (e4.stack-reorder-lambda 2 (1 0 1)))

(e4.word-register
 'ROT (e4.stack-reorder-lambda 3 (2 0 1)))

(e4.word-register
 'DEPTH (lambda () (e4.stack-push (length e4.stack))))

;;; printing words

(e4.word-register
 '.. (lambda () (message "%s" (e4.stack-pop))))

(e4.word-register
 '.s (lambda () (message "<%d> %s" (length e4.stack) e4.stack)))

(e4.word-register
 'SEE (lambda ()
	(let* ((word (intern-soft (e4.stack-pop)))
	       (body (gethash word e4.dictionary)))
	  (if body
	      (message "%s: %s" word body)
	    (message "word `%s' is not defined" word)))))

;;; flow controlling words

(e4.word-register
 'IF (lambda ()
       (when (= e4.FALSE (e4.stack-pop))
	 (e4.skip-tokens-until (or (eq 'ENDIF token)
				   (eq 'ELSE token))))))

(e4.word-register
 'ELSE (lambda ()
	 (e4.skip-tokens-until (or (eq 'ENDIF token)
				   (eq 'ELSE token)))))

(e4.word-register
 'ENDIF (e4.do-nothing))

;;; sequence operations (vectors & strings)

(e4.word-register
 'NTH (lambda ()
	(let ((index (e4.stack-pop)))
	  (e4.stack-push (elt (car e4.stack) index)))))

(e4.word-register
 'LEN (lambda ()
	(e4.stack-push (length (car e4.stack)))))

(e4.word-register
 'SET (lambda ()
	(let ((value (e4.stack-pop)) (index (e4.stack-pop)))
	  (aset (car e4.stack) index value))))

(e4.word-register
 'SPLIT (lambda ()
	  (setq e4.stack (append (e4.stack-pop) e4.stack))))

(e4.word-register
 'VEC (e4.n-aggregation-lambda (cond ((stringp n) (vconcat n))
				      ((> n 0) (vconcat (e4.stack-npop n)))
				      ((< n 0) (make-vector (abs n) 0)))))

(e4.word-register
 'STR (e4.n-aggregation-lambda (cond ((vectorp n) (concat n))
				      ((> n 0) (concat (e4.stack-npop n)))
				      ((< n 0) (make-string (abs n) 0)))))

;;;; advanced api ;;;;

;; it is included into E4 package only temporary.
;; those functions are completely optional, so
;; wise choice lies in separation. 
(load-file
 (expand-file-name "xe4.el" (file-name-directory (or load-file-name
						     buffer-file-name))))
