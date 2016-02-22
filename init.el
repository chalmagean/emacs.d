(add-to-list 'load-path "~/.emacs.d/vendor/")

;; Default to home dir
(cd "~/")

;; Using MELPA for packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Load necessary packages
(package-initialize)
(setq my-required-packages
      (list 'magit
            'restclient
            'change-inner
            'which-key
            'neotree
            'jade-mode
            'alchemist
            'simpleclip
            'ledger-mode
            'helm-ag
            'helm-descbinds
            'helm-swoop
            'projectile
            'helm-projectile
            'counsel
            'jsx-mode
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

;; Display full path in the window title bar
(setq-default frame-title-format '((:eval (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name)) "%f"))))

;; Disable the scroll bar
(scroll-bar-mode 0)

(defun my-ruby-quotes-toggler ()
  "Setup mode local bindings to toggle to various ruby quotes"
  (local-set-key (kbd "C-\"") 'ruby-tools-to-double-quote-string)
  (local-set-key (kbd "C-'") 'ruby-tools-to-single-quote-string)
  (local-set-key (kbd "C-:") 'ruby-tools-to-symbol))

;; Disable the menu when running emacs in the terminal
(unless (display-graphic-p)
  (menu-bar-mode 0))

(require 'anzu) ;; Shows a count of search matches
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(require 'iso-transl) ;; Enable accents in GUI
(require 'goto-chg) ;; Go to last change

(require 'slim-mode)
(add-hook 'slim-mode-hook
          (lambda ()
            (whitespace-mode 0)
            (my-ruby-quotes-toggler)))

;; Set fill-column
(setq-default fill-column 80)

;; Enable SmartScan
(smartscan-mode 1)

(defun find-non-ascii-characters ()
  "It finds the first non-ascii characters from the point forward"
  (interactive)
  (re-search-forward "[^[:ascii:]]"))

;; Prettier line between vertical splits
(set-face-background 'vertical-border "gray")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

;; No splash screen
(setq inhibit-startup-screen t)

;; Simpleclip
(require 'simpleclip)
(simpleclip-mode 1)


;; Set the cursor type to a bar
(setq-default cursor-type 'bar)

;; Don't warn me when opening large files
(setq large-file-warning-threshold nil)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; Enable IDO
;; (load "~/.emacs.d/my-ido.el")

;; To get rid of Weird color escape sequences in Emacs.
;; Instruct Emacs to use emacs term-info not system term info
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)

;; Disable the warning about undo limits
;;(add-to-list 'warning-suppress-types '(undo discard-info))

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
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

;; Smart spell check in HTML
;; http://blog.binchen.org/posts/effective-spell-check-in-emacs.html
(defun web-mode-flyspell-verify ()
  (let ((f (get-text-property (- (point) 1) 'face))
        thing
        rlt)
    (cond
     ((not (memq f '(web-mode-html-attr-value-face
                     web-mode-html-tag-face
                     web-mode-html-attr-name-face
                     web-mode-constant-face
                     web-mode-doctype-face
                     web-mode-keyword-face
                     web-mode-comment-face ;; focus on get html label right
                     web-mode-function-name-face
                     web-mode-variable-name-face
                     web-mode-css-property-name-face
                     web-mode-css-selector-face
                     web-mode-css-color-face
                     web-mode-type-face
                     web-mode-block-control-face)
                 ))
      (setq rlt t))
     ((memq f '(web-mode-html-attr-value-face))
      (save-excursion
        (search-backward-regexp "=['\"]" (line-beginning-position) t)
        (backward-char)
        (setq thing (thing-at-point 'symbol))
        (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$" thing))
        rlt))
     (t t))
    rlt))

(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)

;; Modern CSS frameworks like Bootstrap make doublons unavoidable.
;; For example, CSS class name "btn btn-default" contains double word "btn".
(defvar flyspell-check-doublon t
  "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")
 (make-variable-buffer-local 'flyspell-check-doublon)

(defadvice flyspell-highlight-incorrect-region (around flyspell-highlight-incorrect-region-hack activate)
  (if (or flyspell-check-doublon (not (eq 'doublon (ad-get-arg 2))))
      ad-do-it))

(defun web-mode-hook-setup ()
  (flyspell-mode 1)
  (setq flyspell-check-doublon nil))
(add-hook 'web-mode-hook 'web-mode-hook-setup)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(require 'flycheck)
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")
;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Coffee Script
(setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)
;; If you want to remove sourcemap file after jumping corresponding point
(defun my/coffee-after-compile-hook (props)
  (sourcemap-goto-corresponding-point props)
  (delete-file (plist-get props :sourcemap)))
(add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)
(add-hook 'coffee-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; Load theme
(load-theme 'wombat)
; Set cursor color to white
(set-cursor-color "#ffffff")

(add-to-list 'custom-theme-load-path "~/cobalt2-emacs")
(load-theme 'cobalt2 t)

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
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; Use the short version for yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Always refresh file contents if they change on disk
(global-auto-revert-mode 1)

;; Setting a default line-height
(setq-default line-spacing 1)

;; Handle colors in M-x shell
(load-library "ansi-color")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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

(require 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)

;; Dired
(load "~/.emacs.d/my-dired")

;; Custom functions
(load "~/.emacs.d/my-functions")

;; Make CMD work like ALT (on the Mac)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'none)
;; (setq mac-option-key-is-meta t)
;; (setq mac-right-option-modifier nil)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-pass-command-to-system nil)
(global-set-key [(super s)] 'save-buffer)
;; (global-set-key [(super w)]
;;                 (lambda () (interactive) (kill-buffer)))

(global-set-key [(super z)] 'undo)
(global-set-key [(super backspace)]
                (lambda nil (interactive) (kill-line 0)))

;; SRGB support
(setq ns-use-srgb-colorspace t)

;; Default frame size
;; (setq initial-frame-alist
;;       '((top . 10) (left . 50) (width . 185) (height . 55)))

;; Making dabbrev a bit nicer
(setq dabbrev-abbrev-skip-leading-regexp ":")
(setq dabbrev-backward-only t)

;; Use frames instead of windows for compilation popups
;; (setq-default display-buffer-reuse-frames t)

;; Format line numbers
(setq linum-format "%4d")

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

;; Project explorer
(require 'neotree)

;; Uniquify buffers
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator " : ")

;; Ruby
(load "~/.emacs.d/my-ruby")

;; Scss
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Enable word wrap
(global-visual-line-mode 1)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Ledger
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

;; Which key
(which-key-mode 1)

;; first window settings
(add-to-list 'initial-frame-alist '(tool-bar-lines . 0))
(add-to-list 'initial-frame-alist '(width . 110))
(add-to-list 'initial-frame-alist '(height . 60))
(add-to-list 'initial-frame-alist '(font . "Meslo LG M DZ-13"))
;(add-to-list 'initial-frame-alist '(foreground-color . "dim gray"))
;(add-to-list 'initial-frame-alist '(background-color . "white smoke"))

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(width . 110))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(font . "Meslo LG M DZ-13"))
;(add-to-list 'default-frame-alist '(foreground-color . "dim gray"))
;(add-to-list 'default-frame-alist '(background-color . "white smoke"))

(require 'whitespace)
(setq whitespace-display-mappings
   ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))

(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'ledger-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            whitespace-mode))

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

(defun open-emacs-init()
  "Opens the init.el file"
  (interactive)
  (let (my-buffer-name buffer-name)
    (find-file (expand-file-name "init.el" user-emacs-directory))))

(defun open-emacs-init-other-frame()
  "Opens the init.el file"
  (interactive)
  (let (my-buffer-name buffer-name)
    (select-frame (make-frame))
    (set-frame-size (selected-frame) 120 50)
    (find-file (expand-file-name "init.el" user-emacs-directory))))

(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(mapc (lambda (x)
        (add-to-list 'completion-ignored-extensions x))
      '(".aux" ".bbl" ".blg" ".exe"".log" ".meta" ".out" ".pdf"
        ".synctex.gz" ".tdo" ".toc" "-pkg.el" "-autoloads.el"
        "Notes.bib" "auto/"))

;; Make helm-buffer show the whole name of each buffer
(with-eval-after-load 'helm-projectile
  (setq helm-buffer-max-length nil))

(require 'helm-descbinds)
(helm-descbinds-mode)

;; Key bindings

;; Use C-, instead of C-x
(define-key global-map (kbd "C-,") ctl-x-map)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "C-c h f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c h b") 'helm-projectile-switch-to-buffer)
(global-set-key (kbd "C-c h a") 'helm-projectile-ag)
(global-set-key (kbd "C-c h B") 'helm-buffers-list)
(global-set-key (kbd "C-c h x") 'helm-M-x)
(global-set-key (kbd "C-c h m") 'helm-mini)
(global-set-key (kbd "C-c h i") 'helm-imenu)

(global-set-key [f8] 'neotree-toggle)

(global-set-key (kbd "<f3>") 'hs-hide-block)
(global-set-key (kbd "<f4>") 'hs-show-block)

(global-set-key (kbd "C-c g x") 'git-extract-number-from-branch-name)
(global-set-key (kbd "C-c g s") 'magit-status)

(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-x =") 'balance-windows)

(global-set-key (kbd "C-c f") 'find-file-in-project)

(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; Open the emacs init file
(global-set-key (kbd "<f2>") 'open-emacs-init)
(global-set-key (kbd "S-<f2>") 'open-emacs-init-other-frame)

(global-set-key (kbd "C-c a") 'helm-ag)
(global-set-key (kbd "C-c j") 'dired-jump)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)
(global-set-key (kbd "C-x C-b") 'helm-buffer-list)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c K") 'kill-buffer-and-window)
(global-set-key (kbd "C-c o") 'vi-open-line-below)
(global-set-key (kbd "C-c O") 'vi-open-line-above)
(global-set-key (kbd "C-c , s") 'rspec-verify-single)
(global-set-key (kbd "C-c , r") 'rspec-rerun)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-<SPC>") 'cycle-spacing)
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
    ("a4f8d45297894ffdd98738551505a336a7b3096605b467da83fae00f53b13f01" "af9761c65a81bd14ee3f32bc2ffc966000f57e0c9d31e392bc011504674c07d6" "73abbe794b6467bbf6a9f04867da0befa604a072b38012039e8c1ba730e5f7a5" "cc16715d803b32ae65fa652867551c03e21587b5a5e3450e66c8f1e22dc5e1f5" default)))
 '(dired-omit-verbose nil)
 '(enh-ruby-deep-indent-paren nil)
 '(feature-cucumber-command "bundle exec cucumber {options} {feature}")
 '(flycheck-highlighting-mode nil)
 '(jsx-indent-level 2)
 '(ledger-highlight-xact-under-point nil)
 '(ledger-reconcile-default-commodity "RON")
 '(magit-emacsclient-executable "/usr/local/bin/emacsclient")
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-restore-window-configuration t)
 '(magit-use-overlays nil)
 '(neo-show-updir-line nil)
 '(neo-smart-open nil)
 '(neo-theme (quote nerd))
 '(neo-window-width 40)
 '(reb-re-syntax (quote string))
 '(rspec-spec-command "rspec")
 '(rspec-use-rvm t)
 '(rspec-use-spring-when-possible nil)
 '(scss-compile-at-save nil)
 '(sentence-end-double-space nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#242424"))))
 '(hl-line ((t (:background "gainsboro"))))
 '(ido-subdir ((t (:foreground "dark gray"))))
 '(neo-banner-face ((t (:foreground "lightblue" :weight normal))))
 '(neo-root-dir-face ((t (:foreground "lightblue" :weight normal))))
 '(web-mode-symbol-face ((t (:foreground "indian red")))))
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
