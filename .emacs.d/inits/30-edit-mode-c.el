;;;; c-mode
;;; Auto insert matching brace
(defun my-c-mode-insert-brace ()
  (interactive)
  (let ((pps (syntax-ppss)))
    (when (and (eolp) (not (or (nth 3 pps) (nth 4 pps))))
      (c-indent-line)
      (insert "\n\n}")
      (c-indent-line)
      (forward-line -1)
      (c-indent-line))))
(defun my-return-binding ()
  (interactive)
  (if (and (not (equal (current-column) 0))
           (char-equal (char-before) ?{))
      (my-c-mode-insert-brace)
    (newline-and-indent)))

