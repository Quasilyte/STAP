#!/usr/bin/emacs --script

(setq *dir* (concat (file-name-directory (or load-file-name buffer-file-name))
		    "../"))

(cond ((not argv) (error "path name expected!"))
      ((not (file-exists-p (car argv))) (error "file not found!")))

(load-file (expand-file-name "stap.el" *dir*))

(xstap:import (essential math))

(with-temp-buffer
  (insert "(xstap: ")
  (insert-file-contents (car argv))
  (goto-char (point-max)) 
  (insert ")")
  (eval-buffer))

