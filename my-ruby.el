(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(require 'ruby-mode)
(require 'inf-ruby)

(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition

(setq projectile-completion-system 'grizzl)
;;(add-hook 'ruby-mode-hook 'robe-mode)

(define-key inf-ruby-minor-mode-map (kbd "C-c C-x") nil)

;; Make _ parts of the "word"
(modify-syntax-entry ?_ "w" ruby-mode-syntax-table)

;; Rubocop
;; (require 'rubocop)
