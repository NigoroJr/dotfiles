;;; 's;;' to insert 'std::' in C++
(defun replace-last-two-with-std ()
  (interactive)
  (delete-char -2)
  (insert "std::")
  (auto-complete))
(defun my-semicolon-expansion ()
  (interactive)
  (if (and (char-equal (char-before (- (point) 0)) ?\;)
           (char-equal (char-before (- (point) 1)) ?s))
      (replace-last-two-with-std)
    (insert ";")))

(add-hook 'c++-mode-common-hook
          (lambda ()
            (define-key c++-mode-base-map (kbd ";") 'my-semicolon-expansion)
            (define-key c++-mode-base-map (kbd "RET") 'my-return-binding)
            (define-key c++-mode-base-map (kbd "C-c C-o") 'insert-newline-above)))

