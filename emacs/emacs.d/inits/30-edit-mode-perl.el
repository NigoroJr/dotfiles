;;;; perl-mode
;; Use CPerl mode
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4
      cperl-brace-offset -4
      cperl-label-offset -4
      cperl-highlight-variables-indiscriminately t)

;;; Hooks
;; perl-completion
(add-hook 'cperl-mode-hook 'perl-completion-hook)
(add-hook 'cperl-mode-hook 'add-yasnippet-ac-sources)
