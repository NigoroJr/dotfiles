;;;; General functions

(defun insert-newline-above ()
  (interactive)
  (beginning-of-line)
  (insert "\n")
  (previous-line))
