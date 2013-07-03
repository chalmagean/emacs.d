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

;; Highlight parenthesis
(show-paren-mode 1)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Dired
(load "~/.emacs.d/my-dired")
;; Magit
(load "~/.emacs.d/my-magit")

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

;; IBuffer
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(setq ibuffer-show-empty-filter-groups nil)

;; Uniquify buffers
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator " : ")

;; Ruby
;; Folding
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition

;; Bind YARI to C-h R
(define-key 'help-command "R" 'yari)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Ack
;; Always prompt for a directory root
(setq ack-and-a-half-prompt-for-directory t)
(setq ack-and-a-half-executable "/usr/local/bin/ack")

;; Evil
(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)
(setq evil-shift-width 2)
(setq evil-want-C-u-scroll t)
 
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

;; Keys
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",a" 'ack-and-a-half)
(define-key evil-normal-state-map ",g" 'magit-status)
(define-key evil-normal-state-map "-" 'delete-other-windows)
(define-key evil-normal-state-map ",d" 'dired-jump)
(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map ",b" 'ido-switch-buffer)

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
