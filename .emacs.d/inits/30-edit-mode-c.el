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

;;; Completion for header files (C/C++ mode)
(defun ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))

;;; Hooks
;; Enable auto-complete-clang-async in CC Mode
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)

(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "RET") 'my-return-binding)
            (define-key c-mode-base-map (kbd "C-c C-o") 'insert-newline-above)))
(add-hook 'c-mode-hook 'add-yasnippet-ac-sources)
;; auto-complete-c-headers
(add-hook 'c-mode-common-hook 'ac-c-headers-init)
;; Go back and forth between header and source file
(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-c a") 'ff-find-other-file)))
