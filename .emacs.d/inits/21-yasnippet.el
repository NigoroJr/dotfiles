;;;; yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        ;; Add any other snippits directory here
        ))
(yas-global-mode t)

;; yasnippet sources for AutoComplete
(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

