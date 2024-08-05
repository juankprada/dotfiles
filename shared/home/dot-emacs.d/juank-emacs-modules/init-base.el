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

(use-package envrc
  :init
  (envrc-global-mode)
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
  )

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

                                        ; Don't autosave.
(setq auto-save-default nil)
;; make copy and paste use the same clipboard as emacs.
(setq select-enable-primary t)
(setq select-enable-clipboard t)

;; Ensure I can use paste from the Mac OS X clipboard ALWAYS (or close)
(when (memq window-system '(mac ns))
  (setq interprogram-paste-function (lambda () (shell-command-to-string "pbpaste"))))

;; sets Sunday to be the first day of the week in calendar
(setq calendar-week-start-day 0 )

(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")
;; save emacs backups in a different directory
;; (some build-systems build automatically all files with a prefix, and .#something.someending breakes that)
;; I'm looking at you Yarn!
(setq backup-directory-alist '(("." . "~/.emacsbackups")))

;; Don't create lockfiles. Many build systems that continously monitor the file system get confused by them (e.g, Quarkus). This sometimes causes the build systems to not work anymore before restarting
(setq create-lockfiles nil)

;; don't use version numbers for backup files
(setq version-control 'never)

;; open unidentified files in text mode
(setq default-major-mode 'text-mode)

;; truncate, truncate truncate!
(set-default 'truncate-lines t)

;; make the region visible (but only up to the next operation on it)
(setq transient-mark-mode t)		

;; don't add new lines to the end of a file when using down-arrow key
(setq next-line-add-newlines nil)

;; use y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use visible bell instead of an annoying beep
(setq visible-bell t)

;; Lets make buffers having unique names and  show path if names are repeated
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Don’t compact font caches during GC
(setq inhibit-compacting-font-caches t)  

;; Deleting files go to OS's trash folder
(setq delete-by-moving-to-trash t)       

(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq word-wrap-by-category t)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

(provide 'init-base)
