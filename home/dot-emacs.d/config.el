(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t)  ; if nil, bold is universally disabled
  (setq doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
  (add-to-list 'solaire-mode-themes-to-face-swap "^doom-"))

(set-face-attribute 'default nil
                    :font "Intone Mono Nerd Font Mono 12"
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Hack Nerd Font Propo 12"
                    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
                    :font "Intone Mono Nerd Font Mono 12"
                    :weight 'medium)
;;(setq-default line-spacing 0.10)

(add-to-list 'default-frame-alist '(font . "Intone Mono Nerd Font Mono 12"))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t)

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Hack Nerd Font Propo 12")
  )

(use-package which-key
  :ensure t)
(which-key-mode)

(use-package doom-modeline
  :ensure t)
(doom-modeline-mode 1)

(use-package dashboard-hackernews
  :ensure t
  :config
  (require 'json))

(use-package dashboard
  :ensure t ;; Install dashboard if not installed
  :init     ;; tweak dashboard config before loading it
  (setq dashboard-banner-logo-title "Greetings my lord, let us hack today!")
  (setq dashboard-startup-banner 'logo) ;; use the standrad emacs logo as banner

  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  ;;(setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  (setq dashboard-icon-type 'all-the-icons) ;; use `all-the-icons' package

  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;;(setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons '((("" " Install system package" " Install system package" (lambda (&rest _) (helm-system-packages))))))
  (setq dashboard-set-week-agenda t)
  (setq dashboard-week-agenda t)
  (setq dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "rocket")
                                  (registers . "database")
                                  (hackernews . "newspaper-o")
                                  ))
  (setq dashboard-week-agenda t)
  (setq dashboard-items '((recents . 10)
                          (agenda . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)
                          (hackernews . 5)
                          ))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

(use-package org
  :ensure t
  :init
  (setq org-log-done t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t!)"  "NEXT(n!)" "|" "DONE(d!)")
                (sequence "IDEA(i!)" "MAYBE(y!)" "STAGED(s!)" "WORKING(k!)" "|" "USED(u!/@)")
                ))))

(use-package ox-md
  :after (org))

(use-package rainbow-mode
  :ensure t
  :config
  (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
    (lambda () (rainbow-mode 1)))
  (my-global-rainbow-mode 1)
  )

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1))

(use-package magit
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (projectile-mode +1)
  (helm-projectile-on))

(use-package helm-system-packages
  :ensure t)

(use-package helm-lsp
  :ensure t)

(use-package helm
  :ensure t
  :after
  (helm-projectile)
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq projectile-completion-system 'helm)  ;; optional fuzzy matchinf for heml-M-x
  (setq helm-split-window-in-side-p           t) ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-move-to-line-cycle-in-source     t) ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-ff-search-library-in-sexp        t) ;; search for library in `require' and `declare-function' sexp.
  (setq helm-scroll-amount                    8) ;; scroll 8 lines other window using M-<next>/M-<prior>
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-M-x-fuzzy-match                  t)
  (setq helm-adaptive-mode                    1)
  (setq helm-buffers-fuzzy-matching           t)
  (setq helm-recentf-fuzzy-match              t)
  )

(use-package helm-gtags
  :ensure t
  :after
  (helm)
  :config
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t) ; auto update gtags on save
  (setq helm-gtags-use-input-at-cursor t)
  (setq helm-gtags-pulse-at-cursor t)
  (setq helm-gtags-prefix-key "\C-cg")
  (setq helm-gtags-suggested-key-mapping t)
  ;; enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'rust-mode-hook 'helm-gtags-mode)
  )

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :ensure t
  :config
  ;; Set company completion to begin at once
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  ;; (setq company-minimum-prefix-length 3)
  )


;; With use-package:
(use-package company-box
  :after
  (company)
  :ensure t
  :config
  ;; (setq company-box-frame-behavior 'point)
  ;; (setq company-box-show-single-candidate t)
  ;; (setq company-box-doc-delay 1)
  :hook
  (company-mode . company-box-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (java-mode . lsp)
         (typescript-mode . lsp-deferred)
         (javascript-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Language specific lsp
(use-package lsp-java
  :config (setq lsp-java-bundles (directory-files "/home/juankprada/.jdtls/server/" t ".jar"))
  :ensure t)

(use-package lsp-java
  :ensure t)

;; if you are helm user
(use-package helm-lsp
  :after
  (helm lsp-mode)
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :after lsp-mode
  :ensure t
  :config
  (dap-auto-configure-mode)
  )

(use-package dap-java) ;;to load the dap adapter for your language
(use-package dap-python
  :config
  (setq dap-python-debugger 'debugpy))



;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package string-inflection
  :ensure t
  :config
  ;; default
  (global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)

  ;; for ruby
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))

  ;; for elixir
  (add-hook 'elixir-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-elixir-style-cycle)))

  ;; for java
  (add-hook 'java-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))

  ;; for python
  (add-hook 'python-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-python-style-cycle)))
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode nil)
  (setq flycheck-highlighting-mode 'lines)
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize))

  )

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'sh-mode #'yas-minor-mode)
  (add-hook 'c-mode-hook #'yas-minor-mode)
  (add-hook 'java-mode-hook #'yas-minor-mode)
  (add-hook 'rust-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  (add-hook 'javascript-mode-hook #'yas-minor-mode)
  (add-hook 'typescript-mode-hook #'yas-minor-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode)
  )

(use-package yasnippet-snippets
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "/usr/bin/markdown")
  (setq markdown-css-paths `(,(expand-file-name "Documents/markdown.css")))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  )

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  )

(use-package json-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  )

(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  )

(use-package haskell-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  )

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-trigger 'on-save)
  (setq rustic-format-on-save-method 'rustic-format-buffer)
  (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'text-mode-hook 'my-display-numbers-hook)
;;(global-display-line-numbers-mode 1)
(global-visual-line-mode -1)
(global-hl-line-mode t)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 32000)
;; Determine the underlying operating system
(if (eq system-type 'darwin) (setq juank-aquamacs t))
(if (eq system-type 'gnu/linux) (setq juank-linux t))
(if (eq system-type 'windows-nt)(setq juank-win32 t))

;; Enable electric pair mode by default
(electric-pair-mode t)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; start full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; set default coding of buffers
(setq default-buffer-file-coding-system 'utf-8-unix)
;; switched from tabs to spaces for indentation
(setq-default indent-tabs-mode nil)
;; also set the indentation level to 4.
(setq-default tab-width 4)
;; Don't autosave.
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
(setq backup-directory-alist '(("." . "~/.emacsbackups")))
;; Don't create lockfiles. Many build systems that continously monitor the file system get confused by them (e.g, Quarkus). This sometimes causes the build systems to not work anymore before restarting
(setq create-lockfiles nil)
;; Enable show-paren-mode (to visualize paranthesis) and make it possible to delete things we have marked
(show-paren-mode 1)
(delete-selection-mode 1)
;; don't use version numbers for backup files
(setq version-control 'never)
;; open unidentified files in text mode
(setq default-major-mode 'text-mode)
;; don't wrap long lines.
(set-default 'truncate-lines t)
;; make the region visible (but only up to the next operation on it)
(setq transient-mark-mode t)

;; don't add new lines to the end of a file when using down-arrow key
(setq next-line-add-newlines nil)

;; make find-file and buffer switch case insensitive
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; use y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; winne-mode helper mode to restore window layout
(when (fboundp 'winner-mode)
  (winner-mode 1))

(setq split-height-threshold 20)
(setq split-width-threshold nil)

;; This only works in Mac
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Smooth Scrolling
(setq scroll-conservatively 101)

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Automatically remove trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ignore bell alarm completely
(setq ring-bell-function 'ignore)

;; Always start Emacs with a split view
;; TODO: We may want to check if its possible to start splitted only under certain conditins
;; like passing a parameter to Emacs or Emacs Client
;; (split-window-horizontally)

;; Colours ("Colors" in some other languages)
;; Give me colours in major editing modes!
(require 'font-lock)
(global-font-lock-mode t)

;; Dont show the GNU splash screen
(setq inhibit-startup-message t)

;; highlight region between point and mark
(transient-mark-mode t)

;; highlight during query
(setq query-replace-highlight t)

;; highlight incremental search
(setq search-highlight t)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
;; redefine the isearch-forward-regexp function
(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)


                                        ; Bright-red TODOs, yellow IMPORTANT and STUDY and green NOTE
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Green" nil nil t nil t nil nil)

;; Get compilation buffer to autoscroll. Always!!!
(setq compilation-scroll-output t)


(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c-mode)
         ("\\.c$"   . c-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ("\\.java$" . java-mode)
         ) auto-mode-alist))

;; setup compile package
(require 'compile)
(setq mode-compile-always-save-buffer-p nil)

(require 'cc-mode)

;; (add-hook 'c-mode-hook 'lsp) ;; Check if we really miss it
(add-hook 'c-mode-hook 'yas-minor-mode)

;; C & C++ tweaks
(require 'cc-mode)

(defun my-c-mode-hook ()
  (defun juank-big-fun-c-compilation-hook ()
    (make-local-variable 'truncate-lines)
    (setq truncate-lines nil)
    )

  (add-hook 'compilation-mode-hook 'juank-big-fun-c-compilation-hook)

  (setq compilation-context-lines 0)
  ;; (setq compilation-error-regexp-alist
  ;;       (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
  ;; 	    compilation-error-regexp-alist))

  (setq compilation-directory-locked nil)

  (when (bound-and-true-p juank-win32)
    (setq juank-makescript "build.bat")
    (setq juank-font "outline-Liberation Mono")
    )

  (when (bound-and-true-p juank-aquamacs)
    (cua-mode 0)
    (setq mac-option-modifier nil)
    (setq mac-command-modifier 'meta)
    (setq x-select-enable-clipboard t)
    (setq special-display-regexps nil)
    (setq special-display-buffer-names nil)
    (define-key function-key-map [return] [13])
    (setq mac-command-key-is-meta t)
    (scroll-bar-mode -1)
    (setq mac-pass-command-to-system nil)
    (setq juank-makescript "./build_mac.sh")
    )

  (when (bound-and-true-p juank-linux)
    (setq juank-makescript "./build_linux.sh")
    )

  ;; GDB setuo
  ;; split windows for gdb screens
  (setq gdb-many-windows t gdb-show-main t)
  ;; C++ indentation style
  (defconst juank-big-fun-c-style
    '((c-electric-pound-behavior   . nil)
      (c-tab-always-indent         . t)
      (c-comment-only-line-offset  . 0)
      (c-hanging-braces-alist      . ((class-open)
                                      (class-close)
                                      (defun-open)
                                      (defun-close)
                                      (inline-open)
                                      (inline-close)
                                      (brace-list-open)
                                      (brace-list-close)
                                      (brace-list-intro)
                                      (brace-list-entry)
                                      (block-open)
                                      (block-close)
                                      (substatement-open)
                                      (statement-case-open)
                                      (class-open)))
      (c-hanging-colons-alist      . ((inher-intro)
                                      (case-label)
                                      (label)
                                      (access-label)
                                      (access-key)
                                      (member-init-intro)))
      (c-cleanup-list              . (scope-operator
                                      list-close-comma
                                      defun-close-semi))
      (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                      (label                 . -4)
                                      (access-label          . -4)
                                      (substatement-open     .  0)
                                      (statement-case-intro  .  4)
                                      (statement-block-intro .  c-lineup-for)
                                      (case-label            .  4)
                                      (block-open            .  0)
                                      (inline-open           .  0)
                                      (topmost-intro-cont    .  0)
                                      (knr-argdecl-intro     . -4)
                                      (brace-list-open       .  0)
                                      (brace-list-intro      .  4)))
      (c-echo-syntactic-information-p . t))
    "Casey's Big Fun C++ Style")

  (c-add-style "BigFun" juank-big-fun-c-style t)

  ;; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; Newline indents, semi-colon doesn't
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))
  ;;  Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)
  (setq split-window-preferred-function 'juank-never-split-a-window)
  ;; Abbrevation expansion
  (abbrev-mode 1)

  (turn-on-font-lock)

  (defun juank-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
        (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
          (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
      (error "Unable to find a corresponding file")))


  (defun juank-find-corresponding-file-other-window ()

    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (juank-find-corresponding-file)
    (other-window -1))

  (setq comment-style 'multi-line)
  (setq comment-start "/* ")
  (setq comment-end " */")
  (setq c-indent-level 4) ; a tab is equivalent to 4 spaces
  (local-set-key  (kbd "C-c a") 'juank-find-corresponding-file-other-window)
  (global-set-key [f9] 'first-error)
  (global-set-key [f10] 'previous-error)
  (global-set-key [f11] 'next-error)



  (defun juank-header-format ()
    "Format the given file as a header file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "#if !defined(")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H)\n")
    (insert "/* ========================================================================\n")
    (insert "   $File: $\n")
    (insert "   $Date: $\n")
    (insert "   $Revision: $\n")
    (insert "   $Creator: Juan Prada $\n")
    (insert "   $Notice: (C) Copyright 2016 by Juan Camilo Prada. All Rights Reserved. $\n")
    (insert "   ======================================================================== */\n")
    (insert "\n")
    (insert "#define ")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H\n")
    (insert "#endif")
    )


  (defun juank-source-format ()
    "Format the given file as a source file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "/* ========================================================================\n")
    (insert "   $File: $\n")
    (insert "   $Date: $\n")
    (insert "   $Revision: $\n")
    (insert "   $Creator: Juan Prada $\n")
    (insert "   $Notice: (C) Copyright 2016 by Juan Camilo Prada. All Rights Reserved. $\n")
    (insert "   ======================================================================== */\n")
    )



  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (juank-source-format))
        ((string-match "[.]cin" buffer-file-name) (juank-source-format))
        ((string-match "[.]h" buffer-file-name) (juank-header-format))
        ((string-match "[.]c" buffer-file-name) (juank-source-format))
        ((string-match "[.]cpp" buffer-file-name) (juank-source-format)))



  (add-to-list 'compilation-error-regexp-alist 'juank-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(juank-devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4)))

  (define-key c-mode-map "\t" 'dabbrev-expand)
  (define-key c-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c-mode-map [C-tab] 'indent-region)
  )

(defun juank-big-fun-compilation-hook ()
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil)
  (setq compilation-context-lines 0)
  )

(add-hook 'compilation-mode-hook 'juank-big-fun-compilation-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)

(add-hook 'java-mode-hook 'yas-minor-mode)

(require 'compile)

(defun my-java-mode-hook ()

  ;; Compilation mode
  (defun juank-fun-java-compilation-hook ()
    (make-local-variable 'truncate-lines)
    (setq truncate-lines nil)
    )
  (add-hook 'compilation-mode-hook 'juank-fun-java-compilation-hook())

  (setq compilation-directory-locked nil)

  ;; (setq juank-makescript "build.xml")
  ;; (setq juank-build-command "ant build -emacs")
  ;; (setq juank-clean-command "ant clean -emacs")
  ;; (setq juank-run-command "ant run -emacs")

  (find-java-build-file-recursive)

  (setq tab-width 4
        indent-tabs-mode nil)

  (define-key c-mode-map "\C-m" 'newline-and-indent)
  ;;  Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)
  (setq split-window-preferred-function 'juank-never-split-a-window)

  (abbrev-mode 1)
  (turn-on-font-lock)

  (setq comment-style 'multi-line)
  (setq comment-start "/* ")
  (setq comment-end " */")
  (setq java-indent-level 4)
  (global-set-key [f9] 'first-error)
  (global-set-key [f10] 'previous-error)
  (global-set-key [f11] 'next-error)
  (define-key c-mode-map "\t" 'dabbrev-expand)
  (define-key c-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c-mode-map [C-tab] 'indent-region)

  )


(add-hook 'java-mode-hook 'my-java-mode-hook)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)

;; Prevent Emacs from being closed with this Key combination
(global-set-key (kbd "C-x C-c") 'dont-kill-emacs)
;; Disable the C-z sleep/suspend key
(global-unset-key (kbd "C-z"))

;; Disable the C-x C-b key, because I use helm (C-x b) instead
;; To be fair this is not stupid but I just dont need it
(global-unset-key (kbd "C-x C-b"))

(global-set-key (kbd "C-c C-g") 'goto-line)

;; Duplicate a line
(global-set-key (kbd "C-c C-d") 'duplicate-line)
;; execute my-isearch-word-at-point with ctrl+* key binding
(global-set-key (kbd "C-*") 'my-isearch-word-at-point)
;; String Inflection
(global-set-key (kbd "C-<return>") 'company-complete)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c C-w") 'whitespace-mode)

(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)
(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [pgup] 'forward-page)
(define-key global-map [pgdown] 'backward-page)
(define-key global-map [C-next] 'scroll-other-window)
(define-key global-map [C-prior] 'scroll-other-window-down)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-p") 'helm-projectile)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "M-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-c C-a" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Comment and uncommpent shortcuts
(global-set-key  (kbd "C-c c") 'comment-region)
(global-set-key  (kbd "C-c u") 'uncomment-region)
;; my shortcut for "go to line"
(global-set-key (kbd "C-c C-g") 'goto-line)
(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "M-m") 'make-without-asking )
(global-set-key (kbd "M-n") 'clean-without-asking)
(global-set-key (kbd "C-c C-r") 'run-without-asking)
;;(define-key global-map "\M-m" 'make-without-asking)
;;(define-key global-map "\C-m c" )

(global-set-key [f3] 'juank-replace-string)
(global-set-key [f8] 'eshell)

(global-set-key (kbd "C-c i") 'string-inflection-cycle)
;; Force to CamelCase
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)
;; Force to lowerCamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)
;; Cycle through Java styles
(global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle)
;; Cycle through underscor
(global-set-key (kbd "C-c _") 'string-inflection-underscore)

(defun juank-never-split-a-window
    ;; "Never, ever split a window.  Why would anyone EVER want you to do that??"
    nil)


(defun juank-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
    ))


(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
  )

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
  )

(defun byte-compile-dotfiles ()
  "Byte compile all Emacs dotfiles."
  (interactive)
  ;; Automatically recompile the entire .emacs.d directory.
  (byte-recompile-directory (expand-file-name config-dir) 0))

(defun byte-compile-user-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    ;;(byte-compile-dotfiles)
    ;; (message "%s compiled" user-init-file)
    ))

;; Prevent C-x C-c to kill emacs!!
(defun dont-kill-emacs()
  "Disable C-x C-c binding execute kill-emacs."
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))



;; function to call a command at a specific directory
(defun in-directory ()
  "reads a directory name (using ido), then runs
        execute-extended-command with default-directory in the given
        directory."
  (interactive)
  (let ((default-directory
          (read-directory-name "in directory: "
                               nil nil t)))
    (call-interactively 'execute-extended-command)))


(defun juank-set-ant-build ()
  (setq juank-makescript "build.xml")
  (setq juank-build-command "ant build -emacs")
  (setq juank-clean-command "ant clean -emacs")
  (setq juank-run-command "ant run -emacs")
  )

(defun juank-set-maven-build ()
  (setq juank-makescript "pom.xml")
  (setq juank-build-command "mvn install")
  (setq juank-clean-command "mvn clean")
  (setq juank-run-command "mvn exec:exec")
  )


(defun find-java-build-file-recursive ()
  "Recursively search for a pom.xml file."
  (interactive)
  (if (or (file-exists-p "pom.xml") (file-exists-p "build.xml") )
      (if (file-exists-p "pom.xml") (juank-set-maven-build)
        (juank-set-ant-build))
    (cd "../")
    (find-java-build-file-recursive)))


(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p juank-makescript) t
    (cd "../")
    (find-project-directory-recursive)))


(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))


(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive)
    (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile juank-build-command))
  (other-window 1))


(defun clean-without-asking()
  "Clean the current build."
  (interactive)
  (if (find-project-directory) (compile juank-clean-command))
  (other-window 1))

(defun run-without-asking()
  "Run the current build."
  (interactive)
  (if (find-project-directory) (compile juank-run-command))
  (other-window 1))


;; Function used to call the compile command at a specific dir
(defun project-compile ()
  "reads a directory name (using ido), then runs
        execute-extended-command with default-directory in the given
        directory."
  (interactive)
  (let ((default-directory
          (read-directory-name "compile in directory: "
                               nil nil t)))
    (call-interactively 'compile)))

                                        ;(global-set-key [f2] 'compile-in-directory)

;; custom grep tool
(defun my-grep ()
  "grep recursively for something.  defaults to item at cursor
          position and current directory."
  (interactive)
  (grep (read-string "run grep as: " (concat "grep -isrni " "\"" (thing-at-point 'symbol) "\"" " .")))
  )
;; shortcut for my grep
(global-set-key (kbd "C-c C-s") 'my-grep)



;; function to remove windows line ending
(defun remove-windows-line-endings ()
  "removes the ^m line endings"
  (interactive)
  (replace-string "\^M" "")
  )


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))


(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))



;; search word at point
(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))


(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))


(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))

;; function to duplicate current line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
