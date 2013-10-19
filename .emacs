(add-to-list 'load-path "~/.emacs.d/")

;; Auto insert matching brace
(defun my-c-mode-insert-brace ()
  (interactive)
  (insert "{")
  (let ((pps (syntax-ppss)))
    (when (and (eolp) (not (or (nth 3 pps) (nth 4 pps))))
      (c-indent-line)
      (insert "\n\n}")
      (c-indent-line)
      (forward-line -1)
      (c-indent-line))))
(add-hook 'c-mode-common-hook
  (lambda ()
    (define-key c-mode-base-map "{" 'my-c-mode-insert-brace)))

;; C-h as backspace
(global-set-key "\C-h" 'delete-backward-char)
;; C-w as delete previous word
(global-set-key "\C-w" 'backward-kill-word)

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Indentation level to 4
(setq c-basic-offset 4)

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

;; Use Emacs term info, not the system
(setq system-uses-term-info nil)

;; Set tab width to 4, use space instead of tab
(setq-default
          c-basic-offset 4
          tab-width 4
	      indent-tabs-mode nil)

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
;;        (:name flex-autopair
;;               :type github
;;               :url "https://github.com/uk-ar/flex-autopair"
;;        )
    )
)

;; AutoComplete
(add-to-list 'load-path "~/.emacs.d/el-get/popup")
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/ac-dict")
(ac-config-default)

;; flex-autopair
;;(add-to-list 'load-path "~/.emacs.d/el-get/flex-autopair")
;;(require 'flex-autopair)
;;(flex-autopair-mode 1)

;; MultiTerm
(add-to-list 'load-path "~/.emacs.d/el-get/multi-term")
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
