;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x)
                   (require 'init-funcs))


;; Set Personal information
(setq user-full-name "Juan Camilo Prada")
(setq user-mail-address "juankprada@gmail.com")

;; Make sure custom themes are considered safe.
(setq custom-safe-themes t)


;; Needed for multilanguage support
;; Specially when pasting Japanese characters into emacs buffers
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(unless sys/win32p
  (set-selection-coding-system 'utf-8))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p (daemonp))
  (use-package exec-path-from-shell
    :custom (exec-path-from-shell-arguments '("-l"))
    :init (exec-path-from-shell-initialize)))

(with-no-warnings
  ;; Key Modifiers
  (cond
   (sys/win32p
    ;; make PC keyboard's Win key or other to type Super or Hyper
    ;; (setq w32-pass-lwindow-to-system nil)
    (setq w32-lwindow-modifier 'super     ; Left Windows key
          w32-apps-modifier 'hyper)       ; Menu/App key
    (w32-register-hot-key [s-t]))
   (sys/macp
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'super
          mac-command-modifier 'meta)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo))))
  ;; Optimization
  (when sys/win32p
    (setq w32-get-true-file-attributes nil   ; decrease file IO workload
          w32-use-native-image-API t         ; use native w32 API
          w32-pipe-read-delay 0              ; faster IPC
          w32-pipe-buffer-size 65536))       ; read more at a time (64K, was 4K)
  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil))

  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x10000)  ; 64kb

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject))

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(setq blink-matching-paren nil)
(setq delete-pair-blink-delay 0.1)
(setq help-window-select t)
(setq next-error-recenter '(4)) ; center of the window
(setq find-library-include-other-files nil) ; Emacs 29
(setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
(setq remote-file-name-inhibit-auto-save t)                 ; Emacs 30
(setq tramp-connection-timeout (* 60 10)) ; seconds
(setq save-interprogram-paste-before-kill t)
(setq mode-require-final-newline 'visit-save)
(setq-default truncate-partial-width-windows nil)
(setq eval-expression-print-length nil)
(setq kill-do-not-save-duplicates t)
(setq duplicate-line-final-position -1 ; both are Emacs 29
      duplicate-region-final-position -1)
(setq scroll-error-top-bottom t)

(provide 'init-base)
