;;;; ace-window
;; Set before smartrep to enable repeatable C-x o
;; UNDER REVIEW: Use C-x o for ace-window (with smartrep) if used often
;(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-n") 'ace-window)
(defvar aw-dispatch-alist
  '((?x aw-delete-window " Ace - Delete Window")
    (?t aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?s aw-split-window-horz " Ace - Split Horz Window")
    (?M delete-other-windows " Ace - Maximize Window")))
