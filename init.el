(setq inhibit-startup-message t)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Adding folders to the load path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor/")

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
            'smartscan
            'guide-key
            'project-explorer
            'eproject
            'evil
            'surround
            's
            'expand-region
            'perspective
            'git-commit-mode
            'git-rebase-mode
            'gitconfig-mode
            'gitignore-mode
            'scss-mode
            'sass-mode
            'ack-and-a-half
            'enh-ruby-mode
            'robe
            'ruby-tools
            'highlight-indentation
            'window-number
            'rhtml-mode
            'dired-details
            'yasnippet
            'ibuffer-vc
            'powerline
            'fill-column-indicator
            'ace-jump-mode
            'ace-jump-buffer
            'enclose
            'rvm
            'ag
            'rinari
            'smartparens
            'web-mode
            'feature-mode
            'auto-compile
            'yaml-mode
            'markdown-mode
            'wrap-region
            'gist
            'rspec-mode
            'undo-tree
            'inf-ruby
            'flx
            'goto-chg
            'fiplr))

(dolist (package my-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

;; Window numbers
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

;; Guide key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(setq guide-key/highlight-command-regexp "rectangle")
(guide-key-mode 1)

;; Project explorer
(require 'project-explorer)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Smart scan
(smartscan-mode 1)

;; Smart parens
(show-smartparens-global-mode +1)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; To get rid of Weird color escape sequences in Emacs.
;; Instruct Emacs to use emacs term-info not system term info
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)

;; Prefer utf-8 encoding
(prefer-coding-system 'utf-8)

;; Go to last change
(require 'goto-chg)

;; Highlight 80 column margin
(require 'fill-column-indicator)
(setq fci-rule-use-dashes nil)
(setq fci-always-use-textual-rule nil)

(require 'highlight-indentation)

;; Web mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

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

;; BS mode
(setq bs-must-show-regexp "^\\*scratch*")
(setq bs-dont-show-regexp "TAGS")
(setq bs-attributes-list 
      '(("" 1 1 left bs--get-marked-string)
        ("M" 1 1 left bs--get-modified-string)
        ("R" 2 2 left bs--get-readonly-string)
        ("Buffer" bs--get-name-length 10 left bs--get-name)
        ("" 1 1 left " ")
        ("File" 12 12 left bs--get-file-name)
        ("" 2 2 left "  ")))

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
;; (load "~/.emacs.d/my-ido")

;; Custom functions
(load "~/.emacs.d/my-functions")

;; Evil stuff
(load "~/.emacs.d/my-evil")

;; Powerline
(require 'powerline)
(powerline-center-evil-theme)

;; Make CMD work like ALT (on the Mac)
(setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'none)
;; (setq mac-option-key-is-meta t)
;; (setq mac-right-option-modifier nil)

;; Choosing a dark theme
;; (load-theme 'base16-default t)
(load-theme 'tango-dark t)

;; Default frame size
(setq initial-frame-alist
      '((top . 150) (left . 300) (width . 120) (height . 55)))
   
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
(set-frame-font "Monaco-13")
;;(set-face-attribute 'default nil :height 130)

;; IBuffer
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-vc-set-filter-groups-by-vc-root)))

;; Uniquify buffers
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator " : ")

;; Fiplr
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" "selenium" "doc" "tmp"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" ".DS_Store" "tags" "TAGS" "*.ru" ".keep"))))

;; Ace jump
(require 'ace-jump-mode)

;; Folding
(setq enh-ruby-program "~/.rvm/rubies/ruby-2.0.0-p195/bin/ruby")
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
;;(require 'ruby-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

;;(define-key enh-ruby-mode-map (kbd "C-c #") 'ruby-interpolate)

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

;; Scss
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Ack
;; Always prompt for a directory root
(require 'ack-and-a-half)
(setq ack-and-a-half-prompt-for-directory t)
(setq ack-and-a-half-executable "/usr/local/bin/ack")

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(defun open-emacs-init-file()
  "Opens the init.el file"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
                                          
(global-set-key (kbd "<f1>") 'open-emacs-init-file)
(global-set-key (kbd "C-c a") 'ack-and-a-half)
(global-set-key (kbd "C-c b") 'ace-jump-buffer)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-c j") 'dired-jump)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c K") 'kill-buffer-and-window)
(global-set-key (kbd "C-c f") 'fiplr-find-file)
(global-set-key (kbd "C-c o") 'vi-open-line-below)
(global-set-key (kbd "C-c O") 'vi-open-line-above)
(global-set-key (kbd "C-c r") 'rspec-verify-single)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)

;; God mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; Load a personal.el file if it exists
;; to be able to override stuff in here
(if (file-exists-p "~/.emacs.d/personal.el")
    (load "personal"))

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "b47a3e837ae97400c43661368be754599ef3b7c33a39fd55da03a6ad489aafee" default)))
 '(fci-dash-pattern 0.75)
 '(magit-emacsclient-executable "/usr/local/bin/emacsclient")
 '(magit-restore-window-configuration t)
 '(magit-server-window-for-commit nil)
 '(scss-compile-at-save nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erb-face ((t nil)))
 '(erb-out-delim-face ((t (:foreground "#aaffff")))))
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
