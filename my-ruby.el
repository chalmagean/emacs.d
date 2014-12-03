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

;; Projectile mode
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)
(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)
(require 'robe)

;; Auto completion with company mode
(push 'company-robe company-backends)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; Make _ parts of the "word"
(modify-syntax-entry ?_ "w" ruby-mode-syntax-table)

(require 'ruby-refactor)
(require 'ruby-hash-syntax)
(require 'ruby-additional)
(require 'ruby-block)
(ruby-block-mode t)
(require 'yard-mode)
(require 'rspec-mode)
(require 'eldoc)
(require 'rubocop)

(add-hook 'ruby-mode-hook 'eldoc-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition
(add-hook 'projectile-mode-hook 'projectile-rails-on)
