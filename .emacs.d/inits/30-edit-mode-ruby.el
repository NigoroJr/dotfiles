;;;; ruby-mode
;;; rsense
(setq rsense-home (expand-file-name "~/.emacs.d/el-get/rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;;; ruby-block
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
(add-hook 'ruby-mode-hook
          (lambda ()
            (ruby-block-mode t)))

;;; Hooks
(add-hook 'ruby-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'ruby-mode-hook 'ws-butler-mode)

;;; ruby-end
(require 'ruby-end)
