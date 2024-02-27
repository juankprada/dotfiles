;;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-
;; Download treesitter language

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5)
  :config

  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("C-M-." . consult-eglot-symbols))))


(provide 'init-lsp)