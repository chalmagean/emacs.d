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

;; Load necessary packages
(package-initialize)
(setq my-required-packages
      (list 'magit
            'ack-and-a-half
            'enh-ruby-mode
            'rhtml-mode
            'dired-details
            'yasnippet
            ;; 'evil
            'rvm
            'yaml-mode
            'markdown-mode
            'gist
            'rspec-mode
            'undo-tree
            'inf-ruby
            'flx
            'goto-chg ;; For evil's g;
            'fiplr))

(dolist (package my-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

;; Go to last change
(require 'goto-chg)

;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Prevent adding the coding line
(setq ruby-insert-encoding-magic-comment nil)

;; Always open split windows horizontally
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; Yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Load custom snippets
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs "~/.emacs.d/snippets")

;; Always ident with 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)

;; Use the short version for yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Always refresh file contents if they change on disk
(global-auto-revert-mode 1)

;; Diable bold and underline faces
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;; Recompile lisp files when changed
(require 'auto-compile)
(auto-compile-on-save-mode 1)

;; Showing whitespace
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        (space-mark nil) ; 32 SPACE, 183 MIDDLE DOT
        (newline-mark 10 [172 10]) ; 10 LINE FEED
        (tab-mark 9 [183 9] [92 9]) ; 9 TAB, MIDDLE DOT
        ))
(global-whitespace-mode -1)

;; Setting a default line-height
(setq-default line-spacing 1)

;; Make _ parts of the "word"
(modify-syntax-entry ?_ "w")

;; Don't save any backup files in the current directory
(setq backup-directory-alist `(("." . "~/.emacs_backups")))


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
;; IDO
(load "~/.emacs.d/my-ido")
;; Evil
;; (load "~/.emacs.d/my-evil")

;; Make CMD work like ALT (on the Mac)
(setq mac-command-modifier 'meta)

;; Choosing a dark theme
(load-theme 'fogus t)

;; Making dabbrev a bit nicer
(setq dabbrev-abbrev-skip-leading-regexp ":")
(setq dabbrev-backward-only t)

;; Show line numbers only in opened files
;; Another option could be: http://www.emacswiki.org/emacs/linum-off.el
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
;; Format line numbers
(setq linum-format "%4d ")

;; Disabling the fringe
(fringe-mode 0)

;; Disabling the toolbar
(tool-bar-mode 0)

;; Font
(set-face-attribute 'default nil :height 130)

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

;; Fiplr
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" "selenium" "doc" "tmp"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" ".DS_Store" "tags" "TAGS"))))


;; Folding
(setq enh-ruby-program "~/.rvm/rubies/ruby-2.0.0-p195/bin/ruby")
(require 'ruby-mode)
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition

;; Rhtml mode
(require 'rhtml-mode)
(add-to-list 'auto-mode-alist '("\\.html.erb?\\'" . rhtml-mode))

;; Bind YARI to C-h R
(define-key 'help-command "R" 'yari)

;; RVM
(require 'rvm)
(rvm-use-default)

;; Cucumber
(require 'feature-mode)
(setq feature-use-rvm t)
(setq feature-cucumber-command "cucumber {options} {feature}")
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Rspec
(require 'rspec-mode)
(setq rspec-use-rake-when-possible nil)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Kills live buffers, leaves some emacs work buffers
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and (not (string-equal name ""))
           (not (string-equal name "*Messages*"))
           (not (string-equal name "*scratch*"))
           (/= (aref name 0) ? )
           (kill-buffer buffer)))
    (setq list (cdr list))))

;; Ack
;; Always prompt for a directory root
(setq ack-and-a-half-prompt-for-directory t)
(setq ack-and-a-half-executable "/usr/local/bin/ack")

(defun open-emacs-init-file()
  "Opens the init.el file"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
                                          
(global-set-key (kbd "<f1>") 'open-emacs-init-file)
(global-set-key (kbd "C-c a") 'ack-and-a-half)
(global-set-key (kbd "C-c b") 'bs-show)
(global-set-key (kbd "C-c d") 'dired-jump)
(global-set-key (kbd "C-c g") 'magit-status)

;; Load a personal.el file if it exists
;; to be able to override stuff in here
(if (file-exists-p "~/.emacs.d/personal.el")
    (load "personal"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
