(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(require 'ruby-mode)
(require 'inf-ruby)
(setq ruby-deep-indent-paren nil)
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition

;; Projectile mode
(require 'projectile)
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(setq projectile-completion-system 'grizzl)
(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

;; Auto completion with company mode
(push 'company-robe company-backends)

(add-hook 'robe-mode-hook 'ac-robe-setup)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(define-key inf-ruby-minor-mode-map (kbd "C-c C-x") nil)

;; Make _ parts of the "word"
(modify-syntax-entry ?_ "w" ruby-mode-syntax-table)

(require 'ruby-refactor)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
(add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch)
(require 'ruby-hash-syntax)
(require 'ruby-additional)
(require 'ruby-block)
(ruby-block-mode t)
(require 'yard-mode)
(require 'rspec-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ehn-ruby-mode-hook 'yard-mode)

(require 'eldoc)
(add-hook 'ruby-mode-hook 'eldoc-mode)
(add-hook 'enh-ruby-mode-hook 'eldoc-mode)
;; Rubocop
;; (require 'rubocop)
