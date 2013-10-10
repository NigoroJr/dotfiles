(add-to-list 'load-path "~/.emacs.d/")

;; load path after installing el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; install el-get if not present
(unless (require 'el-get nil t)
    (url-retrieve
      "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
    (lambda (s)
        (let (el-get-master-branch)
            (end-of-buffer)
            (eval-print-last-sexp)))))
(require 'el-get)

(setq el-get-sources
      '(
        (:name popup
               :type github
               :website "https://github.com/auto-complete/popup-el"
               :pkgname "auto-complete/popup-el"
        )
        (:name auto-complete
               :type github
               :website "https://github.com/auto-complete/auto-complete"
               :pkgname "auto-complete/auto-complete"
        )
        (:name flex-autopair
               :type github
               :url "https://github.com/uk-ar/flex-autopair"
        )
    )
)

;; AutoComplete
(add-to-list 'load-path "~/.emacs.d/el-get/popup")
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/ac-dict")
(ac-config-default)

;; flex-autopair
(add-to-list 'load-path "~/.emacs.d/el-get/flex-autopair")
(require 'flex-autopair)
(flex-autopair-mode 1)

;; C-h as backspace
(global-set-key "\C-h" 'delete-backward-char)
;; C-w as delete previous word
(global-set-key "\C-w" 'backward-kill-word)

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Don't create backup files in current directory
(setq backup-directory-alist
      `((".*" . "~/.emacs.backup")))
;; Safest, but slowest
(setq backup-by-copying t)

;; zsh as shell
(setq shell-file-name "/bin/zsh")

;; Show numbers
(global-linum-mode t)

;; Follow symbolic link
(setq follow-link t)
(setq vc-follow-symlinks t)
