;; init-python.el --- Initialize ruby configurations.	-*- lexical-binding: t -*-

;; Run a Ruby process in a buffer
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

;; Ruby YARD comments
(use-package yard-mode
  :diminish
  :hook (ruby-mode . yard-mode))

;; Ruby refactoring helpers
(use-package ruby-refactor
  :diminish
  :hook (ruby-mode . ruby-refactor-mode-launch))

;; Yet Another RI interface for Emacs
(use-package yari
  :bind (:map ruby-mode-map ([f1] . yari)))

;; RSpec
(use-package rspec-mode
  :diminish
  :autoload rspec-install-snippets
  :hook (dired-mode . rspec-dired-mode)
  :config (with-eval-after-load 'yasnippet
            (rspec-install-snippets)))

(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  ;;(global-rbenv-mode)
  :hook
  ((ruby-mode ruby-ts-mode ) . global-rbenv-mode)
  )

(use-package bundler
  ;; For Ruby package management
  )

(provide 'init-ruby)
