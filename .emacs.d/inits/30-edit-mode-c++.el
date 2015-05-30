;;; 's;;' to insert 'std::' in C++
(defun replace-last-two-with (str)
  "Replaces the last two characters (s;) with str (std::)"
  (interactive)
  (delete-char -2)
  (insert str)
  (auto-complete))

(defun my-semicolon-expansion ()
  (interactive)
  (if (char-equal (char-before (- (point) 0)) ?\;)
      (if (char-equal (char-before (- (point) 1)) ?s)
          (replace-last-two-with "std::")
        (if (char-equal (char-before (- (point) 1)) ?b)
            (replace-last-two-with "boost::")))
    (insert ";")))

;;; Hooks
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c-mode-base-map (kbd ";") 'my-semicolon-expansion)))
(add-hook 'c++-mode-hook 'add-yasnippet-ac-sources)
