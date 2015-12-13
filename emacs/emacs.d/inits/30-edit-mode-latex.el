;;;; LaTeX-mode
;;; Check whether these packages are installed since some systems don't
;;; have LaTeX-related packages installed.

;; Only require auto-complete-auctex if managed by el-get
(if (member 'auto-complete-auctex 'el-get-packages)
    (lambda ()
      (require 'auto-complete-auctex)
      (defun ac-LaTeX-mode-setup ()
        (setq ac-sources
              (append '(ac-source-math-unicode
                        ac-source-math-latex
                        ac-source-latex-commands) ac-sources)))
      (add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)))
;; Only require ac-math if managed by el-get
(if (member 'ac-math 'el-get-packages)
    (lambda ()
      (require 'ac-math)
      (add-to-list 'ac-modes 'latex-mode)))

