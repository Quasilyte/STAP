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

;;;; [ MISCELLANEOUS ] ;;;;
;;; something not so tightly related to the STAP or its components

(defun stap-init-conv-table (conv-table rules)
  (dolist (rule rules)
    (puthash (car rule) (cdr rule) conv-table)))

(defmacro stap-do-nothing ()
  "literally, do nothing at all. used for readability"
  '(lambda ()))

(defun stap-symbol-convert (sym)
  "return modified `sym' if it matches any special pattern, unchanged otherwise"
  (if (= ?& (aref (symbol-name sym) 0))
      '&unnamed
    sym))

(defun stap-intern-symbol (s)
  "convert and intern given string"
  (stap-symbol-convert (intern s)))

(defun stap-get-type (any)
  "like `type-of', but returns `integer' instead of `float'"
  (let ((type (type-of any)))
    (if (eq 'float type)
	'integer
      type)))

;;; [ LAMBDA GENERATORS ]
;;; code generation using Lisp macro

(defmacro stap-non-void-lambda (:pop-sym sym prog)
  `(lambda ()
     (let ((,sym (stap-stack-pop)))
       (stap-stack-push ,prog))))

(defmacro stap-overloaded-op-lambda (type-switch arity)
  "create lambda calling lisp function returned from `type-switch'"
  `(lambda ()
     (let ((top (car stap-stack)))
       (stap-stack-push (stap-call-with-arity ,type-switch 2)))))

(defmacro stap-n-aggregation-lambda (seq-concat seq-make)
  `(stap-non-void-lambda
    :pop-sym n
    (cond ((> n 0) (,seq-concat (stap-stack-npop n)))
	  ((< n 0) (,seq-make (abs n) 0)))))

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
      stap-nest-level 0 ;; counter for nested definitions to find balanced }

      stap-conv-tables (make-hash-table :test 'eq :size 3)
      stap-num-conv-table (make-hash-table :test 'eq :size 3)
      stap-str-conv-table (make-hash-table :test 'eq :size 3)
      stap-vec-conv-table (make-hash-table :test 'eq :size 3))

;; i have no idea how to express [] -> number
(stap-init-conv-table stap-num-conv-table '((integer . identity)
					    (string . string-to-number)))

(stap-init-conv-table stap-str-conv-table '((integer . number-to-string)
					    (string . identity)
					    (vector . concat)))

(stap-init-conv-table stap-vec-conv-table '((integer . vector)
					    (string . vconcat)
					    (vector . identity)))

(puthash 'integer stap-num-conv-table stap-conv-tables)
(puthash 'string stap-str-conv-table stap-conv-tables)
(puthash 'vector stap-vec-conv-table stap-conv-tables)

;;;; [ CORE ] ;;;;

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
  (let ((li '()))
    (while (< 0 n)
      (push (stap-stack-pop) li)
      (setq n (1- n)))
    (nreverse li)))

(defun stap-stack-pop-type (conv-table)
  (let ((top (stap-stack-pop)))
    (funcall (gethash (type-of top) conv-table) top)))

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

(defun stap-dict-remove (sym)
  (remhash sym stap-dict))

(defun stap-dict-store (word fn)
  "insert or replace dictionary entry" 
  (puthash word fn stap-dict))

(defun stap-dict-fetch (sym)
  "return dictionary entry, if it is stored (nil otherwise)"
  (gethash (stap-symbol-convert sym) stap-dict :not-found))

;;; [ WORDS ]
;;; operations defined on `words'

(defun stap-definition-exec (fn)
  "execute (maybe recursively) STAP definition"
  (setq stap-tokens (append fn (cdr stap-tokens))))

(defun stap-word-exec (word)
  "invoke word associated lambda"
  (let ((fn (stap-dict-fetch (stap-symbol-convert word))))
    (if (eq :not-found fn)
	(error (format "undefined e4 word: `%s'" word))
      (if (functionp fn)
	  (stap-next-token-do (funcall fn)) ; predefined word
	(stap-definition-exec fn)))))

(defun stap-call-with-arity (fn arity)
  "call lisp function with args passed from the e4 stack"
  (apply fn (nreverse (stap-stack-npop arity))))

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

(defun stap-new-word-set (sym)
  "put user-defined word in dictionary"
  (setq stap-new-word (cons (stap-symbol-convert sym) nil)))

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
	 (fn (if (= 2 (length binding)) word (car (cdr binding))))
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
 '+ (stap-non-void-lambda
     :pop-sym 1st
     (let ((type (stap-get-type 1st)))
       (funcall (pcase type
		  (`integer '+)
		  (`string 'concat)
		  (`vector 'vconcat))
		1st
		(stap-stack-pop-type (gethash type stap-conv-tables))))))

(stap-dict-store
 '= (stap-non-void-lambda
     :pop-sym 1st
     (let ((type (stap-get-type 1st))
	   (2nd (stap-stack-pop)))
       (if (eq 'integer type)
	   (= 1st 2nd)
	 (equal 1st 2nd)))))

;;; [ PREDEFINED: STACK ]
;;; common ways to work with parameter stack

(stap-dict-store
 'shake (lambda ()
	  (let* ((n (stap-stack-pop))
		 (fmt (mapcar (lambda (c) (- c ?0)) (stap-stack-pop)))
		 (slice (stap-stack-npop n)))
	    (stap-stack-npush (mapcar (lambda (pos)
					(nth pos slice)) fmt)))))

(stap-dict-store 'count (lambda () (stap-stack-push (length stap-stack))))

(stap-dict-store 'pop (lambda () (setq stap-stash (stap-stack-pop))))

(stap-dict-store 'push (lambda () (stap-stack-push stap-stash)))

;;; [ PREDEFINED: DISPLAY ] # deprecated
;;; words useful for debugging and interactive development

(stap-dict-store
 '@describe (lambda ()
	      (let* ((sym (intern-soft (stap-stack-pop)))
		     (body (stap-dict-fetch (stap-symbol-convert sym))))
		(if body
		    (message "%s: %s" sym body)
		  (message "word `%s' is not defined" sym)))))

;;; [ PREDEFINED: CONTROL FLOW ]
;;; conditionals, loops (if any will ever appear, they should be here)

(stap-dict-store
 'if (lambda () (when (= stap-FALSE (stap-stack-pop))
		  (stap-skip-tokens-until (or (eq 'endif token)
					      (eq 'else token))))))

(stap-dict-store
 'else (lambda () (stap-skip-tokens-until (or (eq 'endif token)
					      (eq 'else token)))))

(stap-dict-store 'endif (stap-do-nothing))

;;; [ PREDEFINED: SEQUENCE ]
;;; sequence operations (vectors & strings)

(stap-dict-store
 'nth (stap-non-void-lambda :pop-sym index (elt (car stap-stack) index)))

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

(stap-dict-store 'vec (stap-n-aggregation-lambda vconcat make-vector))

(stap-dict-store 'str (stap-n-aggregation-lambda concat make-string))

(stap-dict-store
 'copy (lambda () (stap-stack-push (copy-sequence (car stap-stack)))))

;;; [ PREDEFINED: ENVIRONMENT ]
;;; provides API for communicating with executing interpreter and OS

(stap-dict-store
 'rename (lambda ()
	   (let* ((syms (mapcar 'stap-intern-symbol (stap-stack-npop 2)))
		  (entry (stap-dict-fetch (car (cdr syms)))))
	     (when (not (eq :not-found entry))
	       (stap-dict-store (car syms) entry)
	       (stap-dict-remove (car (cdr syms)))))))

(stap-dict-store
 'query (lambda ()
	  (stap-stack-push (shell-command-to-string (stap-stack-pop)))
	  (when (not (string= "" (car stap-stack)))
	    (message "> `%s'" (car stap-stack)))))

;;;; [ FRIEND IMPORTS ] ;;;;

;; STAP without it is not very user-friendly
(load
 (expand-file-name "xstap.el" (file-name-directory (or load-file-name
						       buffer-file-name)))
 nil t)
