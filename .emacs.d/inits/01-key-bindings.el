;;;; Defines key bindings in various modes
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
