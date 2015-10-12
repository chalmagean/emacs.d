;; Change the diff colors in magit
(require 'magit)
;; (set-face-foreground 'diff-context "#666666")
;; (set-face-foreground 'diff-added "#00cc33")
;; (set-face-foreground 'diff-removed "#ff0000")
;; (setq magit-diff-added ((t (:foreground "lime green"))))
;; (setq magit-diff-added-highlight ((t (:background "grey9" :foreground "lime green"))))
;; (setq magit-diff-context-highlight ((t (:foreground "grey70"))))
;; (setq magit-diff-file-heading-highlight ((t (:inherit magit-diff-file-heading))))
;; (setq magit-diff-removed ((t (:foreground "tomato"))))
;; (setq magit-diff-removed-highlight ((t (:foreground "tomato"))))
 ;; '(diff-added ((t (:inherit diff-changed :foreground "#00cc33"))))
 ;; '(diff-removed ((t (:inherit diff-changed :foreground "tomato"))))
 
(setq magit-completing-read-function 'magit-ido-completing-read)

;; Enable a right limit of 70 chars for git logs
(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-auto-revert-mode nil)
(provide 'my-magit)
