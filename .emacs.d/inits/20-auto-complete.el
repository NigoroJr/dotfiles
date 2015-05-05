;;;; auto-complete
(require 'auto-complete-config)
;; Complete filename (must come to the beginning of ac-sources)
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")

(setq ac-delay 0.1)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(global-auto-complete-mode t)

;; Apply ac-sources
(ac-config-default)

