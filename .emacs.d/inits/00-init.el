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
(defvar backups_dir "~/.emacs.d/backups/")
(defvar autosaves_dir "~/.emacs.d/autosaves/")
;; Create directories if they don't exist
(if (not (file-directory-p backups_dir))
    (mkdir backups_dir))
(if (not (file-directory-p autosaves_dir))
    (mkdir autosaves_dir))
(setq backup-directory-alist
      `((".*" . ,backups_dir)))
(setq auto-save-file-name-transforms
      `((".*" ,autosaves_dir t)))

;; Save cursor position
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

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
(setq ido-auto-merge-work-directories-length -1)
;; Auto-reload file when changed
(global-auto-revert-mode t)
;; Show full path of file in modeline
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    (propertized-buffer-identification "%12f")
                    (propertized-buffer-identification "%12b")))

