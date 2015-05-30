;;;; Configs for general packages

;;; ace-jump-mode
(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(setq ace-jump-mode-move-keys
      (loop for c from ?a to ?z collect c))
(set-face-foreground 'ace-jump-face-foreground "brightgreen")

;;; ace-isearch
(global-ace-isearch-mode t)

;;; SLIME
(setq inferior-lisp-program "clisp")
(slime-setup '(slime-fancy slime-banner))

;;; smartchr
(require 'smartchr)

;;; cc-mode
(require 'cc-mode)
