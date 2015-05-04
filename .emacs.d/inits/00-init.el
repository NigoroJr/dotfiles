;;;; Basic setups
(setq default-directory "~/")
(setenv "ESHELL" shell-file-name)
(fset 'yes-or-no-p 'y-or-n-p)
;; Highlight matching paren
(show-paren-mode t)
;; Indentation level to 4
(setq c-basic-offset 4)
;; Hide tool bar, menu bar, and scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;;; Backups and autosaves
(setq delete-old-versions t
      ;; Auto-save interval
      auto-save-timeout 1
      auto-save-interval 20
      ;; Safest, but slowest
      backup-by-copying t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(defvar BACKUPS_DIR "~/.emacs.d/backups/")
(defvar AUTOSAVES_DIR "~/.emacs.d/autosaves/")
;; Create directories if they don't exist
(if (not (file-directory-p BACKUPS_DIR))
    (mkdir BACKUPS_DIR))
(if (not (file-directory-p AUTOSAVES_DIR))
    (mkdir AUTOSAVES_DIR))
(setq backup-directory-alist
      `((".*" . ,BACKUPS_DIR)))
(setq auto-save-file-name-transforms
      `((".*" , AUTOSAVES_DIR t)))
;; Show numbers
(global-linum-mode t)
(setq linum-format "%3d ")

;; Show column number
(setq column-number-mode t)
;; Show time on modeline
(setq display-time-day-and-date t)
(display-time-mode t)
;; Make file executable if file has #! at beginning
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; Follow symbolic link
(setq follow-link t)
(setq vc-follow-symlinks t)
;; Save history
(savehist-mode t)
(setq history-length 10000)
;; Enable recursive call in mini-buffer
(setq enable-recursive-minibuffers t)
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
;; Enable fuzzy match
(ido-mode t)
(setq ido-enable-flex-matching t)
;; Show full path of file in modeline
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    (propertized-buffer-identification "%12f")
                    (propertized-buffer-identification "%12b")))

;;;; +--------------------------------------------+
;;;; | STUFF NEEDS TO GO INTO SEPARTE DIRECTORIES |
;;;; +--------------------------------------------+

;; Inherit environment variables
(when (memq window-system '(mac ns))
  (lambda ()
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("SHELL"))))

;; yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        ;; Add any other snippits directory here
        ))
(require 'yasnippet)
(yas-global-mode t)

;; AutoComplete
(require 'auto-complete-config)
;; Complete filename (must come to the beginning of ac-sources)
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(setq ac-delay 0.1)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(global-auto-complete-mode t)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)

;; yasnippet sources for AutoComplete
(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  ;; Adding ac-source-dictionary to last of ac-sources did not
  ;; help move yasnippets source to top of completion list.
  ;; Thus, dictionary is disabled when yasnippet is enabled.
  ;; Another workaround is to edit ac-dictionary-directories.
  (setq ac-sources (remove 'ac-source-dictionary ac-sources)))
(add-hook 'c-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'c++-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'ruby-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'cperl-mode-hook 'add-yasnippet-ac-sources)

;; Clang complete
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable (expand-file-name "~/.emacs.d/el-get/auto-complete-clang-async/clang-complete"))
  (setq ac-sources (append '(ac-source-clang-async) ac-sources))
  (setq ac-clang-cflags '("-std=c++11"))
  (ac-clang-launch-completion-process))
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'c++-mode-common-hook 'ac-cc-mode-setup)

;; Apply ac-sources
(ac-config-default)

;; Only require auto-complete-auctex if managed by el-get
(if (member 'auto-complete-auctex 'my:el-get-packages)
    (lambda ()
      (require 'auto-complete-auctex)
      (defun ac-LaTeX-mode-setup ()
        (setq ac-sources
              (append '(ac-source-math-unicode
                        ac-source-math-latex
                        ac-source-latex-commands) ac-sources)))
      (add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)))
;; Only require ac-math if managed by el-get
(if (member 'ac-math 'my:el-get-packages)
    (lambda ()
      (require 'ac-math)
      (add-to-list 'ac-modes 'latex-mode)))

;; helm
(require 'helm)
(require 'helm-config)
;; autoresizef
(helm-autoresize-mode t)

;;;; replacement of `next-error' and `previous-error'
;; From http://rubikitch.com/2014/11/27/helm-next-error/
;;; resumable helm/anything buffers
(defvar helm-resume-goto-buffer-regexp
  (rx (or (regexp "Helm Swoop") "helm imenu" (regexp "helm.+grep") "helm-ag"
          "occur"
          "*anything grep" "anything current buffer")))
(defvar helm-resume-goto-function nil)
(defun helm-initialize--resume-goto (resume &rest _)
  (when (and (not (eq resume 'noresume))
             (ignore-errors
               (string-match helm-resume-goto-buffer-regexp helm-last-buffer)))
    (setq helm-resume-goto-function
          (list 'helm-resume helm-last-buffer))))
(advice-add 'helm-initialize :after 'helm-initialize--resume-goto)
(defun anything-initialize--resume-goto (resume &rest _)
  (when (and (not (eq resume 'noresume))
             (ignore-errors
               (string-match helm-resume-goto-buffer-regexp anything-last-buffer)))
    (setq helm-resume-goto-function
          (list 'anything-resume anything-last-buffer))))
(advice-add 'anything-initialize :after 'anything-initialize--resume-goto)

;; ace-isearch
(global-ace-isearch-mode t)

;; Only apply color theme when in GUI
(when window-system
  (load-theme 'solarized-light t))

;; Save cursor position
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Use Ricty Discord
(if (eq system-type 'gnu/linux)
    (set-frame-font "RictyDiscord-9"))
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
(defun perl-completion-hook ()
  (when (require 'perl-completion nil t)
    (perl-completion-mode t)
    (when (require 'auto-complete nil t)
      (auto-complete-mode t)
      (make-variable-buffer-local 'ac-sources)
      (setq ac-sources
            '(ac-source-perl-completion)))))
(add-hook 'cperl-mode-hook 'perl-completion-hook)

;; rsense
(setq rsense-home (expand-file-name "~/.emacs.d/el-get/rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
(add-hook 'ruby-mode-hook
          (lambda ()
            (ruby-block-mode t)))

;; ace-jump-mode
(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(setq ace-jump-mode-move-keys
      (loop for c from ?a to ?z collect c))
(set-face-foreground 'ace-jump-face-foreground "brightgreen")

;; SLIME
(setq inferior-lisp-program "clisp")
(slime-setup '(slime-fancy slime-banner))

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
