;; Inhibit startup screen
(setq inhibit-startup-message t)

;; Adding folders to the load path
(add-to-list 'load-path "~/.emacs.d/")

;; Using MELPA for packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Don't save any backup files in the current directory
(setq backup-directory-alist `(("." . "~/.emacs_backups")))

;; Don't use tabs to indent
(setq-default indent-tabs-mode nil)
;; Default tabs should be 2 spaces
(setq-default tab-width 2)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Dired
(load "~/.emacs.d/my-dired")
;; Magit
(load "~/.emacs.d/my-magit")
;; Evil
(require 'evil)
(evil-mode 1)

;; Make CMD work like ALT (on the Mac)
(setq mac-command-modifier 'meta)

;; Choosing a dark theme
(load-theme 'tango-dark)

;; Show line numbers only in opened files
;; Another option could be: http://www.emacswiki.org/emacs/linum-off.el
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
;; Format line numbers
(setq linum-format "%4d \u2502 ")

;; Disabling the fringe
(fringe-mode 0)

;; Disabling the toolbar
(tool-bar-mode 0)

;; Keys
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c j") 'dired-jump)
