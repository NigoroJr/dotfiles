;;;; Key bindings for non-window system
;;; multiple-cursors
;; C-M-SPC does not work, so set something different
(global-set-key (kbd "C-c C-m C-c") 'mc/mark-all-dwim-or-mark-sexp)
