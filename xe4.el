;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `xe4' is Extras for Emacs Forth ;;;;;

;; this is addition to `e4' package so it requires
;; loaded Emacs Forth before inclusion

(defun xe4:stack-flush ()
  "make E4 stack empty"
  (setq e4.stack '()))

(defmacro xe4:with-stack-rollback (&rest forms)
  "save stack state, evaluate forms, then restore stack state"
  (let ((stack-state (make-symbol "stack-backup")))
    (let ((stack-state e4.stack))
      `(progn ,@forms)
      `(setq e4.stack ',stack-state))))

(defmacro xe4:with-empty-stack (&rest forms)
  "flush the stack and evaluate given forms"
  (e4:stack-flush)
  `(progn ,@forms))
