;; enable evil leader
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)

(setq evil-shift-width 2)
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)
(setq evil-complete-all-buffers nil)

(require 'evil)
(evil-mode 1)
(setq evil-default-cursor t)
(set-cursor-color "DarkCyan")
(global-evil-tabs-mode t)

;; Clear insert state bindings.
;; (setcdr evil-insert-state-map nil)

;; Set the initial evil state that certain major modes will be in.
(evil-set-initial-state 'magit-log-edit-mode 'emacs)
(evil-set-initial-state 'nav-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)
(evil-set-initial-state 'ibuffer-mode 'normal)

(require 'surround)
(global-surround-mode 1)

(require 'evil-visualstar)
(require 'evil-jumper)
;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)

; BS-menu
(defadvice bs-mode (before bs-mode-override-keybindings activate)
  ;; use the standard bs bindings as a base
  (evil-make-overriding-map bs-mode-map 'normal t)
  (evil-define-key 'normal bs-mode-map "h" 'evil-backward-char)
  (evil-define-key 'normal bs-mode-map "q" 'bs-abort)
  (evil-define-key 'normal bs-mode-map "j" 'bs-down)
  (evil-define-key 'normal bs-mode-map "k" 'bs-up)
  (evil-define-key 'normal bs-mode-map "l" 'evil-forward-char)
  (evil-define-key 'normal bs-mode-map "RET" 'bs-select))

;; Make HJKL keys work in special buffers
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
;;(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings occur-mode 'emacs)

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

;; Evil Keys
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;;(define-key evil-normal-state-map (kbd "C-v") 'evil-scroll-down)

;; Split and move the cursor to the new split
(define-key evil-normal-state-map (kbd "-")
  (lambda ()
    (interactive)
    (split-window-vertically)
    (other-window 1)))
(define-key evil-normal-state-map (kbd "|")
  (lambda ()
    (interactive)
    (split-window-horizontally)
    (other-window 1)))

(define-key evil-normal-state-map (kbd "C-c") 'kill-buffer-and-window)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  "w" 'save-buffer
  "h" 'evil-search-highlight-persist-remove-all
  "a" 'ack-and-a-half
  "g" 'magit-status
  "j" 'dired-jump
  "SPC" 'evil-buffer
  "F" 'find-file
  "f" 'projectile-find-file
  "b" 'bs-show
  "B" 'ibuffer
  "x" 'execute-extended-command
  "d" 'kill-this-buffer
  "q" 'kill-buffer-and-window
  )

;; RSPEC, requires rspec-mode
(evil-declare-key 'normal ruby-mode-map (kbd ",ss") 'rspec-verify-single)
(evil-declare-key 'normal ruby-mode-map (kbd ",sv") 'rspec-verify)
(evil-declare-key 'normal ruby-mode-map (kbd ",sa") 'rspec-verify-all)
(evil-declare-key 'normal ruby-mode-map (kbd ",st") 'rspec-toggle-example-pendingness)
(evil-declare-key 'normal ruby-mode-map (kbd ",sg") 'rspec-toggle-spec-and-target)
(evil-declare-key 'normal rspec-mode-map (kbd ",ss") 'rspec-verify-single)
(evil-declare-key 'normal rspec-mode-map (kbd ",sv") 'rspec-verify)
(evil-declare-key 'normal rspec-mode-map (kbd ",sa") 'rspec-verify-all)
(evil-declare-key 'normal rspec-mode-map (kbd ",st") 'rspec-toggle-example-pendingness)
(evil-declare-key 'normal rspec-mode-map (kbd ",sg") 'rspec-toggle-spec-and-target)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-normal-state-map "\C-j"  'evil-window-down)
(define-key evil-normal-state-map "\C-k"  'evil-window-up)
(define-key evil-normal-state-map "\C-h"  'evil-window-left)
(define-key evil-normal-state-map "\C-l"  'evil-window-right)
