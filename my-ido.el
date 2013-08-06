;; Ido
(require 'ido) ;; loading a newer version which fixes flex matching
(ido-mode)
;; (setq ido-enable-flex-matching t)

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
