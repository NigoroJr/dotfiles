;;;; smartrep
(require 'smartrep)
;;; Switch windows
(smartrep-define-key global-map "C-x"
  '(("o" . other-window)
    ("O" . previous-multiframe-window)
    ("w" . ace-window)))

;;; Window size
(smartrep-define-key global-map "C-x"
  '(("{" . shrink-window-horizontally)
    ("}" . enlarge-window-horizontally)
    ("+" . balance-windows)))
(smartrep-define-key global-map "C-c w"
  '(("h" . shrink-window-horizontally)
    ("l" . enlarge-window-horizontally)
    ("+" . balance-windows)))

;;; Text size
(smartrep-define-key global-map "C-c w"
  '(("+" . text-scale-increase)
    ("-" . text-scale-decrease)
    ("0" . (text-scale-adjust 0))))
(smartrep-define-key global-map "<f2>"
  '(("+" . text-scale-increase)
    ("-" . text-scale-decrease)
    ("0" . (text-scale-adjust 0))))
(smartrep-define-key global-map "<f2>"
  '(("g" . text-scale-increase)
    ("l" . text-scale-decrease)
    ("0" . (text-scale-adjust 0))))

;;; multiple-cursors
(smartrep-define-key global-map "C-c m"
  '(("n" . mc/mark-next-like-this)
    ("p" . mc/mark-previous-like-this)
    ("N" . mc/unmark-next-like-this)
    ("P" . mc/unmark-previous-like-this)))
