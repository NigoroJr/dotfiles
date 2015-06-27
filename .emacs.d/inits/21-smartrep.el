;;;; smartrep
(require 'smartrep)
;;; Switch windows
(smartrep-define-key global-map "C-x"
  '(("o" . other-window)
    ("O" . previous-multiframe-window)
    ("0" . delete-window)
    ("1" . delete-other-windows)
    ("2" . split-window-below)
    ("3" . split-window-right)
    ("+" . balance-windows)
    ("w" . ace-window)))
(smartrep-define-key global-map "C-c w"
  '(("h" . windmove-left)
    ("j" . windmove-down)
    ("k" . windmove-up)
    ("l" . windmove-right)))

;;; Window size
(smartrep-define-key global-map "C-x"
  '(("{" . shrink-window-horizontally)
    ("}" . enlarge-window-horizontally)
    ("+" . balance-windows)))
(smartrep-define-key global-map "C-c w"
  '(("H" . shrink-window-horizontally)
    ("J" . shrink-window)
    ("K" . enlarge-window)
    ("L" . enlarge-window-horizontally)
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
