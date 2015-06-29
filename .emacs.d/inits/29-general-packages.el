;;;; Configs for general packages
;;; ace-jump-mode
(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(setq ace-jump-mode-move-keys
      (loop for c from ?a to ?z collect c))
(set-face-foreground 'ace-jump-face-foreground "brightgreen")
(global-set-key (kbd "C-o") 'ace-jump-char-mode)

;;; ace-isearch
(global-ace-isearch-mode t)

;;; SLIME
(setq inferior-lisp-program "clisp")
(slime-setup '(slime-fancy slime-banner))

;;; smartchr
(require 'smartchr)

;;; cc-mode
(require 'cc-mode)

;;; Projectile
(require 'projectile)
(projectile-global-mode)

;;; visual-regexp-steroids
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-M-r") 'vr/isearch-backward)
(define-key global-map (kbd "C-M-s") 'vr/isearch-forward)

;;; ws-butler
(require 'ws-butler)
;; Hooks defined in 30-edit-mode-*.el
