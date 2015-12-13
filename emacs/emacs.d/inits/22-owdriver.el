;;;; owdriver
(require 'owdriver)
(global-unset-key (kbd "M-o"))
(setq owdriver-prefix-key "M-o")
(owdriver-config-default)
(global-set-key (kbd "M-O") 'owdriver-previous-window)
(owdriver-add-keymap "o" 'owdriver-next-window)
(owdriver-add-keymap "O" 'owdriver-previous-window)
(owdriver-add-keymap "M-o" 'owdriver-next-window)
(owdriver-add-keymap "M-O" 'owdriver-previous-window)
;; Workaround for the issue where the second M-v input results in
;; exiting out of smartrep, and entering a 'v' in the current window.
;; This issue is only observed in terminal (urxvt) hence the display-graphic-p
(if (eq (display-graphic-p) nil)
    (owdriver-add-keymap "v" 'owdriver-do-scroll-down-command))

(owdriver-mode t)
