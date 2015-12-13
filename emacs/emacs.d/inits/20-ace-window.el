;;;; ace-window
;; Set before smartrep.el key bindings definitions
(global-set-key (kbd "C-x w") 'ace-window)
(defvar aw-dispatch-alist
  '((?x aw-delete-window " Ace - Delete Window")
    (?t aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?s aw-split-window-horz " Ace - Split Horz Window")
    (?M delete-other-windows " Ace - Maximize Window")))
