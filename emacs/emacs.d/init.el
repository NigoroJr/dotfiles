;;;; init.el
;;; This file installs el-get.el if not present, and uses init-loader to load
;;; configurations for the various packages installed.

;; Load path after installing el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; Install el-get if not present
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(setq el-get-sources
      '(
        (:name auto-complete-clang-async
               :type github
               :website "https://github.com/Golevka/emacs-clang-complete-async"
               :pkgname "Golevka/emacs-clang-complete-async"
               :build ("make")
               )
        (:name exec-path-from-shell
               :type github
               :website "https://github.com/purcell/exec-path-from-shell"
               :pkgname "purcell/exec-path-from-shell"
               )
        (:name rsense
               :type http-tar
               :url "http://cx4a.org/pub/rsense/rsense-0.3.tar.bz2"
               :options ("xjf")
               :website "http://cx4a.org/software/rsense/index"
               :pkgname "m2ym/rsense"
               )
        ))

;; Packages to install
(setq el-get-packages
      '(
        ac-math
        ac-slime
        ace-isearch
        ace-jump-mode
        ace-window
        ;auctex
        auto-complete
        auto-complete-auctex
        auto-complete-c-headers
        auto-complete-clang-async
        coffee-mode
        color-theme
        color-theme-solarized
        emacs-async
        exec-path-from-shell
        haml-mode
        helm
        helm-ag
        helm-descbinds
        helm-swoop
        inf-ruby
        init-auctex
        init-auto-complete
        init-loader
        multiple-cursors
        owdriver
        popup
        projectile
        projectile-rails
        rsense
        ruby-block
        ruby-end
        scss-mode
        slim-mode
        slime
        smartchr
        smartrep
        visual-regexp-steroids
        ws-butler
        yasnippet
        ))

;; Load any local-only elisps
(if (file-exists-p "~/.localrc/el-get")
    (load-file "~/.localrc/el-get"))
;; TODO: packages.ignore to ignore packages

(el-get 'sync el-get-packages)

(setq init-loader-show-log-after-init nil)
;; Use init-loader to load configurations
(init-loader-load "~/.emacs.d/inits")
