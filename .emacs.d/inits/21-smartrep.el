;;;; smartrep
(require 'smartrep)
(smartrep-define-key global-map "C-x"
  '(("{" . shrink-window-horizontally)
    ("}" . enlarge-window-horizontally)
    ("o" . other-window)
    ;("o" . ace-window)
    ("O" . previous-multiframe-window)))

;;; multiple-cursors
(smartrep-define-key global-map "C-c m"
  '(("n" . mc/mark-next-like-this)
    ("p" . mc/mark-previous-like-this)
    ("N" . mc/unmark-next-like-this)
    ("P" . mc/unmark-previous-like-this)))
