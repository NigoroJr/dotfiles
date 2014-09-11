(add-to-list 'load-path "~/.emacs.d/")
(setq default-directory "~/")

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
;; (global-set-key "\C-w" 'backward-kill-word)

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Indentation level to 4
(setq c-basic-offset 4)

;; Hide tool bar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Use temporary file directory for backups and autosaves
(setq backup-directory-alist
      `((".*" ., temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*", temporary-file-directory)))

;; Safest, but slowest
(setq backup-by-copying t)

;; zsh as shell
(setq shell-file-name "/bin/zsh")

;; Show numbers
;;(global-linum-mode t)

;; Make file executable if file has #! at beginning
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Follow symbolic link
(setq follow-link t)
(setq vc-follow-symlinks t)

;; Use Emacs term info, not the system
(setq system-uses-term-info nil)

;; Use Japanese
(set-language-environment 'Japanese)

;; Use utf-8
(prefer-coding-system 'utf-8)

;; Don't show startup message
(setq inhibit-startup-message t)

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
        (:name auto-complete-clang-async
               :type github
               :website "https://github.com/Golevka/emacs-clang-complete-async"
               :pkgname "Golevka/emacs-clang-complete-async"
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

(if (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Clang complete
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete-clang-async")
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable (expand-file-name "~/.emacs.d/el-get/auto-complete-clang-async/clang-complete"))
  (setq ac-sources (append '(ac-source-clang-async) ac-sources))
  (setq ac-clang-cflags '("-std=c++11"))
  (ac-clang-launch-completion-process))
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'c++-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)

;; Anything
(add-to-list 'load-path "~/.emacs.d/el-get/anything")
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 100
   anything-quick-update t)

   (when (require 'anything-config nil t)
     (setq anything-su-or-sudo "sudo"))
   )

;; Solarized color theme
(add-to-list 'load-path "~/.emacs.d/el-get/color-theme")
(add-to-list 'load-path "~/.emacs.d/el-get/color-theme-solarized")
(require 'color-theme)
(require 'color-theme-solarized)

;; Only apply color theme when in GUI
(when window-system
  (load-theme 'solarized-light t))
;;(load-theme 'solarized-dark t))

;; save cursor position
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Use Ricty Discord
(if (eq system-type 'gnu/linx)
  (set-frame-font "Ricty Discord-9"))
(if (eq system-type 'darwin)
  (set-frame-font "RictyDiscord-13"))

;; Use CPerl mode
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4
      cperl-brace-offset -4
      cperl-label-offset -4
      cperl-highlight-variables-indiscriminately t
      )
;; perl-completion
(add-to-list 'load-path "~/.emacs.d/el-get/perl-completion")
(defun perl-completion-hook ()
  (when (require 'perl-completion nil t)
    (perl-completion-mode t)
    (when (require 'auto-complete nil t)
      (auto-complete-mode t)
      (make-variable-buffer-local 'ac-sources)
      (setq ac-sources
            '(ac-source-perl-completion)))))
(add-hook 'cperl-mode-hook 'perl-completion-hook)

;; flex-autopair
;;(add-to-list 'load-path "~/.emacs.d/el-get/flex-autopair")
;;(require 'flex-autopair)
;;(flex-autopair-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(cursor-color "#839496")
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#839496")
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(if (file-exists-p "~/.emacs_local") (load-file "~/.emacs_local") nil)
