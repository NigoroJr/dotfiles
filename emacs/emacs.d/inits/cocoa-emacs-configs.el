;;; Use Ricty Discord
(set-frame-font "RictyDiscord-13")

;;; exec-path-from-shell
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs
 '("SHELL"))
