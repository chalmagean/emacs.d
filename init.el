(add-to-list 'load-path "~/.emacs.d/vendor/")

;; Default to home dir
(cd "~/")

;; Using MELPA for packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Load necessary packages
(package-initialize)
(setq my-required-packages
      (list 'magit
            'solarized-theme
            'column-enforce-mode
            'swiper ;; visual regex search
            'expand-region
            'wrap-region
            'exec-path-from-shell
            'visual-regexp
            'buffer-move ;; used for rotating buffers
            'visual-regexp-steroids
            'ido-vertical-mode
            'yafolding
            's
            'coffee-mode
            'git-timemachine ;; Walk through git revisions of a file
            'sourcemap
            'slim-mode
            'bundler
            'projectile
            'projectile-rails
            'scss-mode
            'sass-mode
            'f
            'jump
            'discover
            'fiplr
            'yard-mode ;; fontification in ruby comments
            'goto-gem ;; Open dired in a gem directory
            'ruby-tools
            'ruby-block
            'ruby-additional
            'ruby-hash-syntax
            'ruby-refactor
            'dired-details
            'yasnippet
            'yari
            'ibuffer-vc
            'rvm
            'rinari
            'web-mode
            'feature-mode
            'auto-compile
            'yaml-mode
            'rspec-mode
            'undo-tree
            'inf-ruby
            'smartscan ;; Quickly jumps between other symbols found at point in Emacs
            'discover-my-major
            'goto-chg
            'anzu
            'fullframe
            ))

(dolist (package my-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

;; Set PATH, MANPATH and exec-path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Emacs's version of `tail -f`
;; or you can use M-x auto-revert-tail-mode
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Misc information, notably battery state and function name
                " "
                mode-line-misc-info
                ;; And the modes, which I don't really care for anyway
                " " mode-line-modes mode-line-end-spaces))

;; for smooth scrolling and disabling the automatical recentering of emacs when moving the cursor
(setq scroll-margin 5
      scroll-preserve-screen-position 1)

;; Display full path in the window title bar
(setq-default frame-title-format '((:eval (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name)) "%f"))))

;; Disable the scroll bar
(scroll-bar-mode 0)

(require 'anzu) ;; Shows a count of search matches
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(require 'iso-transl) ;; Enable accents in GUI
(require 'goto-chg) ;; Go to last change
(require 'fullframe)
(fullframe magit-status magit-mode-quit-window nil)
(require 'slim-mode)

;; Set fill-column
(setq-default fill-column 80)

;; Enable SmartScan
(smartscan-mode 1)

;; No splash screen
(setq inhibit-startup-screen t)

;; Don't warn me when opening large files
(setq large-file-warning-threshold nil)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; Enable IDO
(load "~/.emacs.d/my-ido.el")

;; To get rid of Weird color escape sequences in Emacs.
;; Instruct Emacs to use emacs term-info not system term info
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)

;; Prefer utf-8 encoding
(prefer-coding-system 'utf-8)

;; Display information about the current function
(require 'eldoc)

;; Configure hippie-expand
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ))

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; Coffee Script
(setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)
;; If you want to remove sourcemap file after jumping corresponding point
(defun my/coffee-after-compile-hook (props)
  (sourcemap-goto-corresponding-point props)
  (delete-file (plist-get props :sourcemap)))
(add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)

;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; Choosing a dark theme
;; (load-theme 'base16-default t)
;; (load-theme 'tango-dark t)
;;(load-theme 'wilson t)
(load-theme 'solarized-dark t)

;; Always open split windows horizontally
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; Stop that bell sound
(setq visible-bell nil)
(setq ring-bell-function (lambda () (message "*beep*")))

;; Yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Load custom snippets
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs "~/.emacs.d/snippets")
;; Disable skeletons in projectile-rails
(setq projectile-rails-expand-snippet nil)

;; Always ident with 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; Use the short version for yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Always refresh file contents if they change on disk
(global-auto-revert-mode 1)

;; Diable bold and underline faces
(when (display-graphic-p)
  (menu-bar-mode 1)
  (set-face-attribute 'default nil :foreground "gray" :font "Inconsolata-13" :height 155))

;; ;; Showing whitespace
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
;;(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark lines-tail))
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark face lines-tail))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        (space-mark nil) ; 32 SPACE, 183 MIDDLE DOT
        (newline-mark 10 [172 10]) ; 10 LINE FEED
        (tab-mark 9 [183 9] [92 9]) ; 9 TAB, MIDDLE DOT
        ))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq whitespace-global-modes '(not org-mode web-mode "Web" emacs-lisp-mode))
(global-whitespace-mode)

;; Setting a default line-height
(setq-default line-spacing 1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

;; Custom functions
(load "~/.emacs.d/my-functions")

;; Make CMD work like ALT (on the Mac)
(setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'none)
;; (setq mac-option-key-is-meta t)
;; (setq mac-right-option-modifier nil)

;; SRGB support
(setq ns-use-srgb-colorspace t)

;; Default frame size
(setq initial-frame-alist
      '((top . 10) (left . 50) (width . 185) (height . 55)))
   
;; Making dabbrev a bit nicer
(setq dabbrev-abbrev-skip-leading-regexp ":")
(setq dabbrev-backward-only t)

;; Use frames instead of windows for compilation popups
;; (setq-default display-buffer-reuse-frames t)

;; Format line numbers
(setq linum-format "%4d ")

;; M-f should move to the beginning of the next word
(require 'misc)

;; Turn on the left fringe
(set-fringe-mode '(10 . 0)) ;; 10px left, 0px right

;; Remove the fringe indicators
(when (boundp 'fringe-indicator-alist)
  (setq-default fringe-indicator-alist
		'(
		  (continuation . nil)
		  (overlay-arrow . nil)
		  (up . nil)
		  (down . nil)
		  (top . nil)
		  (bottom . nil)
		  (top-bottom . nil)
		  (empty-line . nil)
		  (unknown . nil))))

;; Disabling the toolbar
(tool-bar-mode 0)

;; IBuffer
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(setq ibuffer-show-empty-filter-groups nil)
;;(add-to-list 'ibuffer-never-show-predicates "TAGS")
;;(add-to-list 'ibuffer-never-show-predicates "*Backtrace*")
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-vc-set-filter-groups-by-vc-root)))

;; Uniquify buffers
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator " : ")

;; Ruby
(load "~/.emacs.d/my-ruby")

;; Discover mode
(require 'discover)
(global-discover-mode 1)

;; Scss
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)

(defun open-emacs-init-file()
  "Opens the init.el file"
 (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; Key bindings
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-F") 'forward-word)
(global-set-key (kbd "<f3>") 'hs-hide-block)
(global-set-key (kbd "<f4>") 'hs-show-block)

(global-set-key (kbd "C-c g x") 'git-extract-number-from-branch-name)
(global-set-key (kbd "C-c g s") 'magit-status)

(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-x C-5") 'toggle-frame-split)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "<f2>") 'open-emacs-init-file)
(global-unset-key (kbd "C-c C-x"))
(global-set-key (kbd "C-c C-x") 'execute-extended-command)
(global-set-key (kbd "C-c a") 'ag)
(global-set-key (kbd "C-c j") 'dired-jump)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "C-x C-b") 'helm-buffer-list)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c K") 'kill-buffer-and-window)
(global-set-key (kbd "C-c o") 'vi-open-line-below)
(global-set-key (kbd "C-c O") 'vi-open-line-above)
(global-set-key (kbd "C-c , s") 'rspec-verify-single)
(global-set-key (kbd "C-c , r") 'rspec-rerun)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-<SPC>") 'cycle-spacing)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)
(global-set-key (kbd "<down>") (ignore-error-wrapper 'windmove-down))
(global-set-key (kbd "<up>") (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "<left>") (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "<right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window-horizontally)
(define-key isearch-mode-map (kbd "C-<return>") 'isearch-exit-other-end)

;; Load a personal.el file if it exists
;; to be able to override stuff in here
(if (file-exists-p "~/.emacs.d/personal.el")
    (load "~/.emacs.d/personal.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "cdc7555f0b34ed32eb510be295b6b967526dd8060e5d04ff0dce719af789f8e5" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "af9761c65a81bd14ee3f32bc2ffc966000f57e0c9d31e392bc011504674c07d6" "a4f8d45297894ffdd98738551505a336a7b3096605b467da83fae00f53b13f01" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "b47a3e837ae97400c43661368be754599ef3b7c33a39fd55da03a6ad489aafee" default)))
 '(feature-cucumber-command "bundle exec cucumber {options} {feature}")
 '(magit-emacsclient-executable "/usr/local/bin/emacsclient")
 '(magit-restore-window-configuration t)
 '(magit-server-window-for-commit nil)
 '(magit-use-overlays nil)
 '(rspec-spec-command "rspec")
 '(rspec-use-rvm t)
 '(scss-compile-at-save nil)
 '(sentence-end-double-space nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))
 '(diff-added ((t (:inherit diff-changed :foreground "#00cc33"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "tomato"))))
 '(ediff-even-diff-C ((t nil)))
 '(ediff-odd-diff-B ((t (:background "dark blue"))))
 '(ediff-odd-diff-C ((t nil)))
 '(erb-face ((t nil)))
 '(erb-out-delim-face ((t (:foreground "#aaffff"))))
 '(error ((t (:foreground "pink2" :underline nil :weight normal))))
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white"))))
 '(magit-item-highlight ((t nil)))
 '(vertical-border ((((type tty)) (:inherit gray9))))
 '(web-mode-html-attr-name-face ((t (:foreground "dark gray" :underline nil :weight normal))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "gray58" :underline nil :weight normal))))
 '(web-mode-html-tag-face ((t (:foreground "dark cyan" :underline nil :weight normal)))))
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
