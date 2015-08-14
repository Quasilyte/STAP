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

;;; [ CONSTANTS ]
;;; named bindings for literals (mostly for readability)

(defconst stap-TRUE -1 "Forth usually returns -1 as truth value")
(defconst stap-FALSE 0 "only zero is considered as falsy value")
(defconst stap-DEFAULT-STASH-SIZE 8 "capacity of global accessible vector")
(defconst stap-UNNAMED '&unnamed "symbol used to distinguish unnamed function")

;;; [ RUNTIME VARIABLES ]
;;; state of the STAP environment in those

(setq stap-stack '() ; main data stack for execution
      stap-stash [] ; fixed-size vector stored out of main stack
      stap-tokens '() ; usually this is an input data for interpreter
      stap-new-word '() ; car is word-symbol, cdr is its body
      stap-dict (make-hash-table :test 'equal) ; all defined words
      stap-eval-mode :interpret ;; can be `:interpret' or `:compile'
      stap-nest-level 0) ;; counter for nested definitions to find balanced }

;;;; [ MISCELLANEOUS ] ;;;;
;;; something not so tightly related to the STAP or its components

(defun stap-symbol-convert (sym)
  "return modified `sym' if it matches any special pattern, unchanged otherwise"
  (if (= ?& (aref (symbol-name sym) 0))
      stap-UNNAMED
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

(defmacro stap-dict-defun (sym io prog)
  "wrapper for registering dictionary entries"
  (let ((in (car io)) (out (car (cdr io))))
    (when out
      (setq prog (list (if (listp out)
			   'stap-stack-npush
			 'stap-stack-push)
		       prog)))
    (setq in (mapcar (lambda (name)
		       (if (listp name)
			   (let ((name (car name)) (n (car (cdr name))))
			     (list name (list 'stap-stack-npop n)))
			 (list name '(stap-stack-pop))))
		     (if (listp in)
			 in
		       (list in))))
    (list 'stap-dict-store
	  sym
	  (list 'lambda nil
		(if in
		    `(let ,in ,prog)
		  prog)))))

;;; [ WORDS ]
;;; operations defined on `words'

(defun stap-definition-exec (sym)
  "execute (maybe recursively) STAP definition"
  (setq stap-tokens (append fn ; function body (new instructions)
			    (cdr stap-tokens)))) ; rest instructions

(defun stap-word-exec (sym)
  "invoke word associated lambda"
  (let ((fn (stap-dict-fetch (stap-symbol-convert sym))))
    (if (eq :not-found fn)
	(error (format "undefined e4 word: `%s'" sym))
      (if (functionp fn)
	  (stap-next-token-do (funcall fn)) ; predefined word
	(stap-definition-exec sym)))))

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
  (let* ((new-word (nreverse stap-new-word)))
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

(defun stap-stash-init (stash-size)
  (let ((stash-size (if stash-size
			stash-size
		      stap-DEFAULT-STASH-SIZE)))
    (if (= stash-size (length stap-stash))
	(fillarray stap-stash 0) 
      (setq stap-stash (make-vector stash-size 0)))))

(defun stap-reload-environment (tokens stash-size)
  "prepare STAP environment to run another portion of expressions"
  (stap-stash-init stash-size)
  (stap-dict-store stap-UNNAMED '()) ; reset unnamed word
  (setq stap-eval-mode :interpret) ; we must always start from this mode
  (setq stap-tokens tokens))

(defun stap: (words &optional stash-size)
  "take some STAP words as a list, evaluate them"
  (stap-reload-environment words stash-size)
  (stap-while-token
   (if (listp token)
       (stap-next-token) ; if it is a list, then it is a comment
     (if (eq :interpret stap-eval-mode) ; else process token in current mode
	 (stap-interpreting-eval token)
       (stap-compiling-eval token)))))

;;; [ QUERY ]
;;; handle STAP requests made by `query' word

(defun stap-query-bash (msg)
  "pass `msg' to bash interpreter, return printed output"
  (let ((output (shell-command-to-string msg)))
    (when (not (string= "" output)) 
      (message "> `%s'" output)) ; not empty output is printed
    (stap-stack-push output)))

(defun stap-command-describe (name)
  "print internal representation of `name' definition contents"
  (let* ((sym (intern-soft name))
	 (body (stap-dict-fetch (stap-symbol-convert sym))))
    (if body
	(message "`%s' %s" sym body)
      (message "word `%s' is not defined" sym))))

(defun stap-command-dump (target)
  "print dumped `target'"
  (message "TBA"))

(defun stap-query-command (msg)
  "parse `msg' and execute special command"
  (let* ((msg (split-string msg))
	 (cmd (car msg))
	 (arg (car (cdr msg))))
    (funcall (pcase cmd
	       ("describe" 'stap-command-describe)
	       ("dump" 'stap-command-dump))
	     arg)))

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
(stap-dict-defun
 '! (top t) (if (and (numberp top) (zerop top))
		stap-TRUE
	      stap-FALSE))
		  
(stap-dict-defun '+ ((1st 2nd) t) (+ 1st 2nd))

(stap-dict-defun '= ((1st 2nd) t) (let ((type (stap-get-type 1st)))
				    (if (eq 'integer type)
					(= 1st 2nd)
				      (equal 1st 2nd))))

;;; [ PREDEFINED: STACK ]
;;; common ways to work with parameter stack

(stap-dict-defun
 'shake (n (t)) (let ((fmt (mapcar (lambda (c) (- c ?0)) (stap-stack-pop)))
		      (slice (stap-stack-npop n)))
		  (mapcar (lambda (pos)
			    (nth pos slice)) fmt)))

(stap-dict-defun 'count (nil t) (length stap-stack))

(stap-dict-defun 'stash (nil t) stap-stash)

;;; [ PREDEFINED: CONTROL FLOW ]
;;; conditionals, loops (if any will ever appear, they should be here)

(stap-dict-defun
 'if (nil nil) (when (= stap-FALSE (stap-stack-pop))
		 (stap-skip-tokens-until (or (eq 'endif token)
					     (eq 'else token)))))

(stap-dict-defun
 'else (nil nil) (stap-skip-tokens-until (or (eq 'endif token)
					     (eq 'else token))))

(stap-dict-defun 'endif (nil nil) '())

;;; [ PREDEFINED: SEQUENCE ]
;;; sequence operations (vectors & strings)

(stap-dict-defun 'nth (index t) (elt (car stap-stack) index))

(stap-dict-defun 'len (nil t) (length (car stap-stack)))

(stap-dict-defun
 'set ((val index) nil) (aset (car stap-stack) index val))

(stap-dict-defun 'split (seq nil) (setq stap-stack (append seq stap-stack)))

(stap-dict-defun 'vec (n t) (cond ((vectorp n) (vconcat n (stap-stack-pop)))
				  ((> n 0) (vconcat (stap-stack-npop n)))
				  ((< n 0) (make-vector (abs n) 0))))

(stap-dict-defun 'str (n t) (cond ((stringp n) (concat n (stap-stack-pop)))
				  ((> n 0) (concat (stap-stack-npop n)))
				  ((< n 0) (make-string (abs n) 0))))

(stap-dict-defun 'copy (nil t) (copy-sequence (car stap-stack)))

;;; [ PREDEFINED: TYPE ASSERTS ]
;;; check the top element type, convert to desired type if its type differs

(stap-dict-defun
 'num! (top t) (pcase (stap-get-type top)
		 (`integer top)
		 (`string (string-to-number top))
		 (`vector (error "vector can not be coerced with `num!'"))))

(stap-dict-defun
 'str! (top t) (pcase (stap-get-type top)
		 (`integer (number-to-string top))
		 (`string top)
		 (`vector (error "vector can not be coerced with `str!'"))))

(stap-dict-defun
 'vec! (top t) (if (vectorp top)
		   top
		 (vector top)))

;;; [ PREDEFINED: ENVIRONMENT ]
;;; provides API for communicating with executing interpreter and OS
 
(stap-dict-defun
 'rename (((names 2)) nil) (let* ((syms (mapcar 'stap-intern-symbol names))
				  (entry (stap-dict-fetch (car (cdr syms)))))
			     (when (not (eq :not-found entry))
			       (stap-dict-store (car syms) entry)
			       (stap-dict-remove (car (cdr syms))))))

(stap-dict-defun
 'query (msg nil) (if (= ?$ (aref msg 0))
		      (stap-query-bash (substring msg 1))
		    (stap-query-command msg)))

;;;; [ FRIEND IMPORTS ] ;;;;

;; STAP without it is not very user-friendly
(load
 (expand-file-name "xstap.el" (file-name-directory (or load-file-name
						       buffer-file-name)))
 nil t)
