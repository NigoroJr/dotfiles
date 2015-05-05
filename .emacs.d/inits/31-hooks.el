;;;; Hooks for various modes

;;; auto-complete
;(add-hook 'auto-complete-mode-hook 'ac-common-setup)
;; Enable auto-complete-clang-async in CC Mode
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)

(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "RET") 'my-return-binding)
            (define-key c-mode-base-map (kbd "C-c C-o") 'insert-newline-above)))
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c-mode-base-map (kbd ";") 'my-semicolon-expansion)))

;; perl-completion
(add-hook 'cperl-mode-hook 'perl-completion-hook)

;; yasnippet
(add-hook 'cperl-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'ruby-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'c++-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'c-mode-hook 'add-yasnippet-ac-sources)
