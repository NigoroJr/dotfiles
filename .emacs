;; Don't create backup files in current directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Show numbers
(global-linum-mode t)
(setq linum-format "%d ")

;; Follow symbolic link
(setq follow-link t)
(setq vc-follow-symlinks t)

;; Enable auto indent
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-m" 'indent-new-comment-line)

;; Kill whole line with C-k when at the beginning
(setq kill-whole-line t)

;; Use zsh as shell
(setq shell-file-name "/usr/local/bin/zsh")

;; auto-complete
(add-to-list 'load-path "~/.emacs.d")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-modes (cons 'js-mode ac-modes))