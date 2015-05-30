;;;; multiple-cursors
(require 'multiple-cursors)
(defun mc/edit-lines-or-string-rectangle (s e)
  (interactive "r")
  (if (eq (save-excursion (goto-char s) (current-column))
          (save-excursion (goto-char e) (current-column)))
      (call-interactively 'mc/edit-lines)
    (call-interactively 'string-rectangle)))

(defun mc/mark-all-dwim-or-mark-sexp (arg)
  (interactive "p")
  (cl-case arg
    (16 (mc/mark-all-dwim t))
    (4 (mc/mark-all-dwim nil))
    (1 (mark-sexp 1))))

;;; Key bindings
;; Note: urxvt does not recognize C-M-SPC
(global-set-key (kbd "C-M-SPC") 'mc/mark-all-dwim-or-mark-sexp)
(global-set-key (kbd "C-x r t") 'mc/edit-lines-or-string-rectangle)

(global-set-key (kbd "C-c C-m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-m N") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-c C-m P") 'mc/unmark-previous-like-this)
