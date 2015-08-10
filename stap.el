;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `STAP' is STAck Processing language ;;;;;

;; package naming convention:
;; e4:name -- exported (public) binding
;; stap-name -- unexported (private) binding
;;
;; e.g. `stap:' is exported, `stap-stack' is not 

;; word -- anything in `stap-dict'
;; tag -- something look like word, but defined at implementation level
;;
;; e.g. `{' and `}' are tags, but `IF' is a word

;;;; [ DATA SECTION ] ;;;;

(defconst stap-TRUE -1 "Forth usually returns -1 as truth value")
(defconst stap-FALSE 0 "only zero is considered as falsy value")

;; STAP environment variables
(setq stap-stack '() ; main data stack for execution
      stap-stash '() ; pop'ed elements goes in here
      stap-tokens '() ; usually this is an input data for interpreter
      stap-new-word '() ; car is word-symbol, cdr is its body
      stap-dict (make-hash-table :test 'equal) ; all defined words
      stap-eval-mode :interpret ;; can be `:interpret' or `:compile'
      stap-nest-level 0) ;; counter for nested definitions to find balanced }

;;;; [ CORE ] ;;;;

;;; [ MISCELLANEOUS ]
;;; something not so tightly related to the STAP or its components

(defmacro stap-do-nothing ()
  "literally, do nothing at all. used for readability"
  '(lambda ()))

;;; [ STACK ]
;;; operations defined on `stack' (also known as data or parameter stack)

(defun stap-stack-drop ()
  "drop top element from the stack"
  (setq stap-stack (cdr stap-stack)))

(defun stap-stack-ndrop (n)
  "drop n top elements from the stack"
  (setq stap-stack (nthcdr n stap-stack)))

(defun stap-stack-pop ()
  "take top element from the stack"
  (pop stap-stack))

(defun stap-stack-npop (n)
  "take n top elements from the stack and return them as a list"
  (prog1
      (subseq stap-stack 0 n)
    (stap-stack-ndrop n)))

(defun stap-stack-push (scalar)
  "push scalar on top of the stack"
  (push (pcase scalar
	  (`t stap-TRUE) 
	  (`nil stap-FALSE)
	  (_ scalar))
	stap-stack))

(defun stap-stack-npush (scalars)
  "push given list of scalars on the data stack"
  (setq stap-stack (append scalars stap-stack)))

;;; [ DICTIONARY ]
;;; operations defined on `dictionary'

(defun stap-dict-store (word fn)
  "insert or replace dictionary entry" 
  (puthash word fn stap-dict))

(defun stap-dict-fetch (word)
  "return dictionary entry, if it is stored (nil otherwise)"
  (gethash word stap-dict :not-found))

;;; [ WORDS ]
;;; operations defined on `words'

(defun stap-word-exec (word)
  "invoke word associated lambda"
  (let ((fn (stap-dict-fetch word)))
    (if (eq :not-found fn)
	(error (format "undefined e4 word: `%s'" word))
      (if (functionp fn)
	  (stap-next-token-do (funcall fn)) ; predefined word
	(setq stap-tokens (append fn (cdr stap-tokens)))))))

(defun stap-call-with-arity (fn arity)
  "call lisp function with args passed from the e4 stack"
  (apply fn (nreverse (stap-stack-npop arity))))

(defmacro stap-overloaded-op-lambda (type-switch arity)
  "create lambda calling lisp function returned from `type-switch'"
  `(lambda ()
     (let ((top (car stap-stack)))
       (stap-stack-push (stap-call-with-arity ,type-switch 2)))))

(defmacro stap-n-aggregation-lambda (n-cond)
  `(lambda ()
     (let ((n (stap-stack-pop)))
       (stap-stack-push ,n-cond))))

;;; [ TOKENS ]
;;; operations defined on `tokens'

(defun stap-next-token ()
  "throw away current token and take another"
  (setq stap-tokens (cdr stap-tokens)))

(defmacro stap-next-token-do (action)
  "select next token, perform `action'. wrapped in `progn'"
  `(progn
     (stap-next-token)
     ,action))

(defmacro stap-while-token (&rest forms)
  "evaluate passed forms through tokens (token binding passed implicitly)"
  `(while stap-tokens
     (let ((token (car stap-tokens)))
       ,@forms)))

(defmacro stap-skip-tokens-until (termination-p)
  "skip current token and keep skipping until `termination-p' is t"
  `(catch 'break
     (stap-while-token
      (stap-next-token)
      (when ,termination-p (throw 'break nil)))))

;;; [ INTERPRET ]
;;; walking through tokens while evaluating them

(defun stap-finish-words-interpretation ()
  "exit interpetation mode and begin new word interning"
  (stap-next-token)
  (setq stap-eval-mode :compile))

(defun stap-take-scalar (scalar)
  "while moving to next token, store given scalar in stack"
  (stap-next-token)
  (stap-stack-push scalar))

(defun stap-interpreting-eval (word)
  "process word while in interpreter mode"
  (if (eq '{ word) ; mode switching symbol
      (stap-finish-words-interpretation)
    (if (symbolp word)
	(stap-word-exec word) 
      (stap-take-scalar word)))) ; everything except symbols considered scalars

;;; [ COMPILE ]
;;; walking through tokens while collecting them without evaluation

(defmacro stap-nest-level-modify (mutator)
  "change `nest-level' value by applying mutator function (usually 1+ or 1-)"
  `(setq stap-nest-level (,mutator stap-nest-level)))

(defun stap-new-word-set (word)
  "put user-defined word in dictionary"
  (setq stap-new-word (cons word nil)))

(defun stap-compilation-closing-tag ()
  "handle compilation terminating tag (it should be balanced with open tag)"
  (cond ((= 0 stap-nest-level) (stap-finish-word-compilation))
	(t (stap-nest-level-modify 1-) ; nested word definition end
	   (push word stap-new-word))))

(defun stap-finish-word-compilation ()
  "finalize word compilation and enter interpretation mode"
  (let ((new-word (nreverse stap-new-word)))
    (stap-dict-store (car new-word) (cdr new-word)))
  (setq stap-eval-mode :interpret
	stap-new-word '()))

(defun stap-compiling-eval (word)
  "process word while in compiler mode"
  (if (eq '} word) ; mode switching symbol
      (stap-compilation-closing-tag)
    (cond (stap-new-word ; if new name is already registered
	   (when (eq '{ word) (stap-nest-level-modify 1+)) ; nested definition ?
	   (push word stap-new-word))
	  (t (stap-new-word-set word)))) ; otherwise, store it
  (stap-next-token))

;;; [ EVALUATION ]
;;; the highest level, entry point

(defun stap-reload-environment (tokens)
  "prepare STAP environment to run expressions"
  (setq stap-eval-mode :interpret) ; we must always start from this mode
  (setq stap-tokens tokens))

(defun stap: (words)
  "take some STAP words as a list, evaluate them"
  (stap-reload-environment words)
  (stap-while-token
   (if (listp token)
       (stap-next-token) ; if it is a list, then it is a comment
     (if (eq :interpret stap-eval-mode) ; else process token in current mode
	 (stap-interpreting-eval token)
       (stap-compiling-eval token)))))

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
		   (neg - 1)))
  (let* ((word (car binding))
	 (fn (if (= 2 (length binding)) word (cadr binding)))
	 (arity (car (last binding))))
    (stap-dict-store
     word `(lambda ()
	     (stap-stack-push (stap-call-with-arity ',fn ,arity))))))

;; only 0 number is considered false (can be changed later)
(stap-dict-store
 '! (lambda ()
      (let ((top (stap-stack-pop)))
	(stap-stack-push (if (and (numberp top) (zerop top))
			     stap-TRUE
			   stap-FALSE)))))

(stap-dict-store
 '+ (stap-overloaded-op-lambda (cond ((numberp top) '+)
				     ((stringp top) 'concat)) 2))

(stap-dict-store
 '= (stap-overloaded-op-lambda (cond ((numberp top) '=)
				     ((stringp top) 'string=)) 2))

;;; [ PREDEFINED: STACK ]
;;; common ways to work with parameter stack

(stap-dict-store
 'shake (lambda ()
	  (let* ((n (stap-stack-pop))
		 (fmt (mapcar (lambda (c) (- c ?0)) (stap-stack-pop)))
		 (slice (stap-stack-npop n)))
	    (stap-stack-npush (mapcar (lambda (pos)
					(nth pos slice)) fmt)))))

(stap-dict-store
 'count (lambda () (stap-stack-push (length stap-stack))))

(stap-dict-store
 'pop (lambda () (setq stap-stash (stap-stack-pop))))

(stap-dict-store
 'push (lambda () (stap-stack-push stap-stash)))

;;; [ PREDEFINED: DISPLAY ]
;;; words useful for debugging and interactive development

(stap-dict-store
 '@one (lambda () (message "%s" (stap-stack-pop))))

(stap-dict-store
 '@all (lambda () (message "<%d> %s" (length stap-stack) stap-stack)))

(stap-dict-store
 '@describe (lambda ()
	      (let* ((word (intern-soft (stap-stack-pop)))
		     (body (stap-dict-fetch word)))
		(if body
		    (message "%s: %s" word body)
		  (message "word `%s' is not defined" word)))))

;;; [ PREDEFINED: CONTROL FLOW ]
;;; conditionals, loops (if any will ever appear, they should be here)

(stap-dict-store
 'if (lambda ()
       (when (= stap-FALSE (stap-stack-pop))
	 (stap-skip-tokens-until (or (eq 'endif token)
				     (eq 'else token))))))

(stap-dict-store
 'else (lambda ()
	 (stap-skip-tokens-until (or (eq 'endif token)
				     (eq 'else token)))))

(stap-dict-store
 'endif (stap-do-nothing))

;;; [ PREDEFINED: SEQUENCE ]
;;; sequence operations (vectors & strings)

(stap-dict-store
 'nth (lambda ()
	(let ((index (stap-stack-pop)))
	  (stap-stack-push (elt (car stap-stack) index)))))

(stap-dict-store
 'len (lambda ()
	(stap-stack-push (length (car stap-stack)))))

(stap-dict-store
 'set (lambda ()
	(let ((value (stap-stack-pop)) (index (stap-stack-pop)))
	  (aset (car stap-stack) index value))))

(stap-dict-store
 'split (lambda ()
	  (setq stap-stack (append (stap-stack-pop) stap-stack))))

(stap-dict-store
 'vec (stap-n-aggregation-lambda (cond ((stringp n) (vconcat n))
				       ((> n 0) (vconcat (stap-stack-npop n)))
				       ((< n 0) (make-vector (abs n) 0)))))

(stap-dict-store
 'str (stap-n-aggregation-lambda (cond ((vectorp n) (concat n))
				       ((> n 0) (concat (stap-stack-npop n)))
				       ((< n 0) (make-string (abs n) 0)))))

(stap-dict-store
 'copy (lambda ()
	 (stap-stack-push (copy-seq (car stap-stack)))))

;;;; [ FRIEND IMPORTS ] ;;;;

;; STAP without it is not very user-friendly
(load-file
 (expand-file-name "xstap.el" (file-name-directory (or load-file-name
						       buffer-file-name))))
