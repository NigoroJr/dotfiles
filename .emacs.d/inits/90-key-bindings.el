;;;; Defines key bindings in various modes

;;;; Global
;;; Traverse history with C-p and C-n
(define-key minibuffer-local-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-map "\C-n" 'next-history-element)
(define-key minibuffer-local-must-match-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-must-match-map "\C-n" 'next-history-element)
(define-key minibuffer-local-completion-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-completion-map "\C-n" 'next-history-element)

;;; C-h as backspace
(define-key key-translation-map [?\C-h] [?\C-?])
;;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)
;;; Go to previous buffer
(define-key global-map (kbd "C-x O") 'previous-multiframe-window)
;;; Insert newline below
(define-key global-map (kbd "C-c o") 'insert-newline-below)
;;; Insert newline above
(define-key global-map (kbd "C-c C-o") 'insert-newline-above)

;;;; Packages
;;; visual-regexp-steroids
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-M-r") 'vr/isearch-backward)
(define-key global-map (kbd "C-M-s") 'vr/isearch-forward)

;;; ace-jump-mode
(global-set-key (kbd "C-o") 'ace-jump-char-mode)
