;;;; Hooks for various modes

;; Wrap lines
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (set-fill-column 78)))
