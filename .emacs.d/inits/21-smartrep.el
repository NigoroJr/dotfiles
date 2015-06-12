;;;; smartrep
(require 'smartrep)
(smartrep-define-key global-map "C-x"
                     '(("{" . shrink-window-horizontally)
                       ("}" . enlarge-window-horizontally)
                       ("o" . other-window)
                       ("O" . previous-multiframe-window)))

;;; Key bindings for multiple-cursors
;; Make sure these key bindings follow what's set for multiple-cursors
(smartrep-define-key global-map "C-c C-m"
                     '(("n" . mc/mark-next-like-this)
                       ("p" . mc/mark-previous-like-this)
                       ("N" . mc/unmark-next-like-this)
                       ("P" . mc/unmark-previous-like-this)))
