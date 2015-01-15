(add-to-list 'load-path "~/.emacs.d/vendor/")

;; Default to home dir
(cd "~/")

;; Using MELPA for packages
(when (>= emacs-major-version 24)
  (require 'package)
  (when (>= emacs-major-version 24)
    (setq package-archives '(("melpa" . "http://melpa.org/packages/")))))

;; Load necessary packages
(package-initialize)
(setq my-required-packages
      (list 'magit
            ;; 'evil
            ;; 'evil-tabs
            ;; 'evil-jumper
            ;; 'evil-visualstar
            ;; 'evil-search-highlight-persist
            ;; 'surround
            ;; 'powerline-evil
            'exec-path-from-shell
            'visual-regexp
            'buffer-move ;; used for rotating buffers
            'visual-regexp-steroids
            'ido-vertical-mode
            'yafolding
            'robe
            's
            'coffee-mode
            'ag
            'git-timemachine
            'sourcemap
            'bundler
            'projectile
            'projectile-rails
            'helm-ack
            'helm-projectile
            'helm-robe
            'rubocop
            'scss-mode
            'sass-mode
            'f
            'jump
            'discover
            'fiplr
            ;;'ack-and-a-half
            'yard-mode ;; fontification in ruby comments
            'ruby-tools
            'persp-projectile
            'ruby-block
            'ruby-additional
            'ruby-hash-syntax
            'ruby-refactor
            'magit-gh-pulls
            'rhtml-mode
            'dired-details
            'yasnippet
            'yari
            'ibuffer-vc
            'fill-column-indicator
            'rvm
            'rinari
            'web-mode
            'feature-mode
            'auto-compile
            'yaml-mode
            'rspec-mode
            'expand-region
            'undo-tree
            'inf-ruby
            'smartscan                          ;; Quickly jumps between other symbols found at point in Emacs
            'discover-my-major
            'goto-chg
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

;; for smooth scrolling and disabling the automatical recentering of emacs when moving the cursor
(setq scroll-margin 1
      scroll-conservatively 0)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Disable the scroll bar
(scroll-bar-mode 0)

;; Expand region
(require 'expand-region)

;; Enable accents in GUI
(require 'iso-transl)

;; Enable SmartScan
(smartscan-mode 1)

;; No splash screen
(setq inhibit-startup-screen t)

;; Don't warn me when opening large files
(setq large-file-warning-threshold nil)

;; Magit GH pulls
(eval-after-load 'magit
  '(define-key magit-mode-map "#gg"
     'endless/load-gh-pulls-mode))

(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; Disable the menu
(menu-bar-mode 0)

;; Enable IDO
(load "~/.emacs.d/my-ido.el")

;; To get rid of Weird color escape sequences in Emacs.
;; Instruct Emacs to use emacs term-info not system term info
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)

;; Prefer utf-8 encoding
(prefer-coding-system 'utf-8)

;; Go to last change
(require 'goto-chg)

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

;; Coffee Script
(setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)
(defun coffee-after-compile-delete-file (props)
  (delete-file (plist-get props :sourcemap)))
(add-hook 'coffee-after-compile-hook 'coffee-after-compile-delete-file t)

;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; Choosing a dark theme
;; (load-theme 'base16-default t)
;; (load-theme 'tango-dark t)
(load-theme 'wilson t)

;; Prevent adding the coding line
(setq ruby-insert-encoding-magic-comment nil)

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
(when (display-graphic-p)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal :underline nil))
   (face-list))
  (menu-bar-mode 1)
  (set-face-attribute 'default nil :foreground "gray" :font "Inconsolata-13" :height 155)
  )

;; ;; Showing whitespace
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        (space-mark nil) ; 32 SPACE, 183 MIDDLE DOT
        (newline-mark 10 [172 10]) ; 10 LINE FEED
        (tab-mark 9 [183 9] [92 9]) ; 9 TAB, MIDDLE DOT
        ))
(global-whitespace-mode 1)

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

;; Evil stuff
;; (load "~/.emacs.d/my-evil")

;; Hide the modeline
(load "~/.emacs.d/hidden-mode-line")

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

;; Show line numbers only in opened files
;; Another option could be: http://www.emacswiki.org/emacs/linum-off.el
;; (add-hook 'find-file-hook (lambda () (linum-mode 1)))

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

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
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

(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;; Use bash
(setq explicit-shell-file-name "/bin/bash")
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))
;; Close the terminal buffer on exit
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

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

;; all buffers, try to reuse windows across all frames
(add-to-list 'display-buffer-alist
           '(".*". (display-buffer-reuse-window .
                                  ((reusable-frames . t)))))

;; except for compilation buffers where you want new and dedicated frames when necessary
(add-to-list 'display-buffer-alist
         '("^\\*Compile-Log\\*". ((display-buffer-reuse-window
                                   display-buffer-pop-up-frame) .
                                  ((reusable-frames . t)
                                   (inhibit-same-window . t)))))

;; Key bindings
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-F") 'forward-word)

(global-set-key (kbd "C-c g x") 'git-extract-number-from-branch-name)
(global-set-key (kbd "C-c g s") 'magit-status)

(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-x C-5") 'toggle-frame-split)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "<f2>") 'open-emacs-init-file)
(global-unset-key (kbd "C-c C-x"))
(global-set-key (kbd "C-c C-x") 'execute-extended-command)
(global-set-key (kbd "C-c C-a") 'ack-and-a-half)
(global-set-key (kbd "C-c j") 'dired-jump)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c K") 'kill-buffer-and-window)
(global-set-key (kbd "C-c o") 'vi-open-line-below)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c O") 'vi-open-line-above)
(global-set-key (kbd "C-c a w") 'ace-jump-word-mode)
(global-set-key (kbd "C-c a l") 'ace-jump-line-mode)
(global-set-key (kbd "C-c a c") 'ace-jump-char-mode)
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
(define-key isearch-mode-map (kbd "C-<return>") 'isearch-exit-other-end)

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
    ("cdc7555f0b34ed32eb510be295b6b967526dd8060e5d04ff0dce719af789f8e5" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "af9761c65a81bd14ee3f32bc2ffc966000f57e0c9d31e392bc011504674c07d6" "a4f8d45297894ffdd98738551505a336a7b3096605b467da83fae00f53b13f01" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "b47a3e837ae97400c43661368be754599ef3b7c33a39fd55da03a6ad489aafee" default)))
 '(feature-cucumber-command "bundle exec cucumber {options} {feature}")
 '(magit-emacsclient-executable "/usr/local/bin/emacsclient")
 '(magit-restore-window-configuration t)
 '(magit-server-window-for-commit nil)
 '(rspec-spec-command "spring rspec")
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-spring-when-possible t)
 '(scss-compile-at-save nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(web-mode-html-tag-face ((t (:foreground "dark cyan" :underline nil :weight normal))))
 '(whitespace-newline ((t (:foreground "#3f3f3f" :weight thin)))))
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
