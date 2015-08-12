#!/usr/bin/emacs --script

(setq *dir* (concat (file-name-directory (or load-file-name buffer-file-name))
		    "../"))

(cond ((not argv) (error "path name expected!"))
      ((not (file-exists-p (car argv))) (error "file not found!")))

(load (expand-file-name "stap.el" *dir*) nil t)

(xstap:import (essential math))

(setq *elapsed* (benchmark-run (with-temp-buffer
				 (insert "(xstap: ")
				 (insert-file-contents (car argv))
				 (goto-char (point-max)) 
				 (insert ")")
				 (eval-buffer))))

(message "[%s seconds]" (- (car *elapsed*) (nth 2 *elapsed*)))

