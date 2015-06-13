;; Change the diff colors in magit
(require 'magit)
(set-face-background 'magit-item-highlight "#121212")
(set-face-foreground 'diff-context "#666666")
(set-face-foreground 'diff-added "#00cc33")
(set-face-foreground 'diff-removed "#ff0000")
(setq magit-server-window-for-commit nil)
(setq magit-emacsclient-executable "/usr/local/bin/emacsclient")

;; Enable a right limit of 70 chars for git logs
(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-auto-revert-mode nil)
(provide 'my-magit)
