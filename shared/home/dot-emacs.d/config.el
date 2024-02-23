;; -*- lexical-binding: t -*-
(setq warning-suppress-log-types '((package reinitialization)))  (package-initialize)
(require 'package)
(setq package-vc-register-as-project nil) ; Emacs 30
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 4)
        ("melpa" . 3)
        ("nongnu" . 2)
        ("org" . 1)))

;; NOTE 2023-08-21: I build Emacs from source, so I always get the
;; latest version of built-in packages.  However, this is a good
;; solution to set to non-nil if I ever switch to a stable release.
(setq package-install-upgrade-built-in nil)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(when (not package-archive-contents)
  (package-refresh-contents))

;; For additional packages not in MELPA or ELPA
(require 'use-package)
(use-package quelpa)
(use-package quelpa-use-package)
(quelpa-use-package-activate-advice)
(setq load-prefer-newer t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize))
  )

(setq user-full-name "Juan Camilo Prada")
(setq user-mail-address "juankprada@gmail.com")

;; Treat custom themes as safe
(setq custom-safe-themes t)

(use-package gcmh
  :ensure t
  ;; *Gcmh* does garbage collection (GC) when the user is idle.
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config (gcmh-mode))


(random t)

;; Disable the Menu bar
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a Win$ux system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a CrapOS system?")

;; Needed for multilanguage support
;; Specially when pasting Japanese characters into emacs buffers
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq indent-line-function 'insert-tab)

(use-package modus-themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-disable-other-themes t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package fontaine
  ;; A narrow focus package for naming font configurations and then selecting
  ;; them.
  :config
  (setq fontaine-presets
        ;; I'm naming the presets as "actions"; the mindset that I'm using when
        ;; wanting that font.
        '((compact
           :default-height 140)
          (default
           :default-height 150)
          (comfy
           :default-height 170)
          (coding
           :default-height 150)
          (presenting
           :default-weight semilight
           :default-height 230
           :bold-weight extrabold)
          (reading
           :default-weight semilight
           :default-family "Hack Nerd Font "
           :default-height 150
           :bold-weight extrabold)
          (t
           ;; Following Prot’s example, keeping these for for didactic purposes.
           :default-family "Hack Nerd Font Mono"
           :default-weight regular
           :default-height 170
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "HAck Nerd Font Propo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))
  (fontaine-set-preset 'default))

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-hud nil)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-time-live-icon t)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-position-line-format nil) ;; We don't need Line number position in modeline
  (setq doom-modeline-minor-modes nil) ;; Don't display minor modes in modeline
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  )

(use-package dired-preview
  :config
  ;; Default values for demo purposes
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                "\\|iso\\|epub\\|pdf\\)"))

  ;; Enable `dired-preview-mode' in a given Dired buffer or do it
  ;; globally:
  (dired-preview-global-mode 1)


  (defun my-dired-preview-to-the-right ()
    "My preferred `dired-preview-display-action-alist-function'."
    '((display-buffer-in-side-window)
      (side . right)
      (width . 0.3)))

  (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-right)
  )

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package kind-icon
  ;; This packages helps provide additional icons for functions and variables in
  ;; the completion candidates.
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as
                                        ; `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and
                                        ; background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  ;; (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ;
  ;; Change cache dir
  :config
                                        ; Enable `kind-icon'
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks
            #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package which-key
  :ensure t)
(which-key-mode)

(use-package iedit
  :ensure t
  :bind ("C-|" . iedit-mode)
  :diminish)

(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  ;; Always delete and copy recursively
  (dired-listing-switches "-lah")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

(use-package fancy-compilation
  :ensure t
  :config
  (setq fancy-compilation-override-colors nil)
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package undo-tree
  :ensure t
  :defer t
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory))))
  (undo-tree-visualizer-timestamps t))

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package avy
  :defer t
  :bind
  (("C-:" . avy-goto-char-timer)
   ("C-;" . avy-goto-line))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face
  (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))));

(use-package crux

  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-x 4 t" . crux-transpose-windows)
   ("C-x K" . crux-kill-other-buffers)
   ("C-k" . crux-smart-kill-line))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package dashboard-hackernews

  :init
  :config
  (require 'json))

(use-package dashboard
  ;; Install dashboard if not installed
  :init     ;; tweak dashboard config before loading it
  (setq dashboard-banner-logo-title "Greetings master. What are we working on today?")
  ;;(setq dashboard-startup-banner 'logo) ;; use the standrad emacs logo as banner
  (setq dashboard-startup-banner (concat "~/.emacs.d/logos/logo-" (number-to-string (random 21)) ".png")) ;; use the standrad emacs logo as banner

  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  ;;(setq dashboard-icon-type 'all-the-icons) ;; use `all-the-icons' package

  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;;(setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-set-navigator t)
  ;;(setq dashboard-navigator-buttons '((("" " Install system package" " Install system package" (lambda (&rest _) (helm-system-packages))))))
  (setq dashboard-set-week-agenda t)
  (setq dashboard-week-agenda t)
  (setq dashboard-heading-icons '((recents   . "nf-oct-history")
                                  (bookmarks . "nf-oct-bookmark")
                                  (agenda    . "nf-oct-calendar")
                                  (projects  . "nf-oct-rocket")
                                  (registers . "nf-oct-database")
                                  (hackernews . "nf-oct-log")
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

  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         (:map org-mode-map (("C-c C-p" . eaf-org-export-to-pdf-and-open)
                             ("C-c ;" . nil))))
  :custom
  (org-log-done 'time)
  (calendar-latitude 35.689487) ;; Prerequisite: set it to your location, currently default: Toronto, Canada
  (calendar-longitude 139.691711) ;; Usable for M-x `sunrise-sunset' or in `org-agenda'
  (org-export-backends (quote (ascii html icalendar latex md odt pdf)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-latex-listings-options '(("breaklines" "true")))
  (setq org-todo-keywords
        (quote ((sequence "TODO(t!)"  "NEXT(n!)" "|" "DONE(d!)")
                (sequence "IDEA(i!)" "MAYBE(y!)" "STAGED(s!)" "WORKING(k!)" "|" "USED(u!/@)")
                )))
  (org-latex-listings t)
  (org-deadline-warning-days 7)
  (org-agenda-window-setup 'other-window)
  (org-latex-pdf-process
   '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :custom-face
  (org-agenda-current-time ((t (:foreground "spring green"))))
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (unless (version< org-version "9.2")
    (require 'org-tempo))
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (C . t)
     (python . t)
     (plantuml . t)))
  (defun org-export-toggle-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq-local org-latex-listings 'minted)
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

  (defun org-table-insert-vertical-hline ()
    "Insert a #+attr_latex to the current buffer, default the align to |c|c|c|, adjust if necessary."
    (interactive)
    (insert "#+attr_latex: :align |c|c|c|")))

(use-package org-roam
  :after org
  :custom
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n h" . org-id-get-create)))
;;:config
;;(when (file-directory-p "~/org/roam/")
;;(setq org-roam-directory (file-truename "~/org/roam")))
;;(org-roam-db-autosync-mode))


(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package ox-md
  :disabled
  :after (org))

(use-package htmlize :defer t)

(use-package rainbow-mode

  :config
  (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
    (lambda () (rainbow-mode 1)))
  (my-global-rainbow-mode 1)
  )

(use-package projectile

  :config
  (projectile-global-mode 1))

(use-package magit

  :if (executable-find "git")
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window)))
  :config
  (defun magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t)))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  (defun my/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  :bind (:map vertico-map
              ("/" . #'my/vertico-insert))
  :config
  (setq vertico-preselect 'directory)
  (defun +vertico-restrict-to-matches ()
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
                           '(invisible t read-only t cursor-intangible t rear-nonsticky t))))
  (define-key vertico-map (kbd "S-/") #'+vertico-restrict-to-matches)

  (defvar +vertico-current-arrow t)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
            (concat #("▶" 0 1 (face vertico-current)) cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat
           #(" " 0 1 (display (left-fringe right-triangle vertico-current)))
           cand)
        cand)))

  )


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))



;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)              ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
      ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
      ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
      ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
      ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
      ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-flycheck
  :after consult)

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-'" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after embark
  ;; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package consult-dir
  ;; This package helps ease traveling across directories by providing directory
  ;; candidates related to current buffers, bookmarks, and projects.  Further,
  ;; like other ~consult.el~ functions, you can use narrowing keys.  See
  ;; https://github.com/karthink/consult-dir.
  :after (consult)
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package consult-projectile
  ;; package provides a function I use everyday: ~M-x consult-projectile~.  When
  ;; I invoke ~consult-projectile~, I have the file completion for the current
  ;; project.  I can also type =b= + =SPACE= to narrow my initial search to open
  ;; buffers in the project.  Or =p= + =space= to narrow to other projects; and
  ;; then select a file within that project.
  :commands (consult-projectile)
  :config
  ;; I want recent files as well as project files as well as recent project
  ;; files...Hence the override fb
  (setq juank/consult--source-recent-file consult--source-recent-file)
  (plist-put juank/consult--source-recent-file :narrow ?R)
  (plist-put juank/consult--source-recent-file :name "Recent File")
  (setq consult-projectile-sources
        '( ;; key b
          consult-projectile--source-projectile-buffer
          ;; key f
          consult-projectile--source-projectile-file
          ;; key p
          consult-projectile--source-projectile-project
          ;; key d
          consult-projectile--source-projectile-dir
          ;; key m
          consult--source-bookmark
          ;; key r
          consult-projectile--source-projectile-recentf
          ;; key R
          juank/consult--source-recent-file
          ;; key *
          consult--source-modified-buffer))

  (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
    (interactive)
    (let ((default-directory (or dir default-directory)))
      (consult--read #'read-file-name-internal :state (consult--file-preview)
                     :prompt prompt
                     :initial initial
                     :require-match mustmatch
                     :predicate pred)))
  :bind ("C-x C-p" . consult-projectile)
  )


(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (style basic partial-completion)))
        ))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?_)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :config
  (setq corfu-auto t
        corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)

  )

;; A few more useful configurations...
(use-package emacs
  :init

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold nil)      ; Always show all candidates in popup menu
  )

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)

  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package treesit
  :ensure nil
  :init
  (setq treesit-font-lock-level 4)
  :init

  (defun juank/treesit/function-select ()
    "Select the current function at point."
    (interactive)
    (if-let ((func (treesit-defun-at-point)))
        (progn
          (goto-char (treesit-node-start func))
          (call-interactively #'set-mark-command)
          (goto-char (treesit-node-end func)))
      (user-error "No function to select")))

  ;; This function, tested against Ruby, will return the module space qualified
  ;; method name (e.g. Hello::World#method_name).
  (cl-defun juank/treesit/yank-qualified-method-fname ()
    "Return the fully qualified name of method at point.  If not on a method, get the containing class."
    (if-let ((func (treesit-defun-at-point)))
        ;; Instance method or class method?
        (let* ((method_type (if (string= "method"
                                         (treesit-node-type func))
                                "#" "."))
               (method_name (treesit-node-text
                             (car (treesit-filter-child
                                   func
                                   (lambda (node)
                                     (string= "identifier"
                                              (treesit-node-type node)))))))
               (module_space (s-join "::" (juank/treesit/module_space func))))
          (if current-prefix-arg
              module_space
            (concat module_space method_type method_name)))
      (let ((current-node (treesit-node-at (point))))
        (s-join "::" (juank/treesit/module_space current-node)))))

  ;; Handles the following Ruby code:
  ;;
  ;;   module A::B
  ;;     module C
  ;;     end
  ;;     C::D = Struct.new do
  ;;       def call
  ;;       end
  ;;     end
  ;;   end
  ;; Special thanks to https://eshelyaron.com/posts/2023-04-01-take-on-recursion.html
  (defun juank/treesit/module_space (node &optional acc)
    (if-let ((parent (treesit-parent-until
                      node
                      (lambda (n) (member (treesit-node-type n)
                                          '("class" "module" "assignment")))))
             (parent_name (treesit-node-text
                           (car
                            (treesit-filter-child
                             parent
                             (lambda (n)
                               (member (treesit-node-type n)
                                       '("constant" "scope_resolution"))))))))
        (juank/treesit/module_space parent (cons parent_name acc))
      acc)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package scopeline
  ;; Show the scope info of methods, blocks, if/case statements.  This is done
  ;; via an overlay for "blocks" that are more than 5 (default) lines
                                        ;:straight (:host github :repo "jeremyf/scopeline.el")
  :hook (prog-mode-hook . scopeline-mode))

;; (defun my-typescript-eglot-hook ()
;;   ((js-mode
;;     typescript-mode
;;     typescriptreact-mode) . eglot-ensure)
;;   ;; I'm not sure why this is needed, but it throws an error if I remove it
;;   (cl-defmethod project-root ((project (head eglot-project)))
;;     (cdr project))
;;   (defun my-project-try-tsconfig-json (dir)
;;     (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
;;       (cons 'eglot-project found)))

;;   (add-hook 'project-find-functions
;;             'my-project-try-tsconfig-json nil nil)

;;   (add-to-list 'eglot-server-programs
;;                '((typescript-mode) "typescript-language-server" "--stdio"))
;;   )

(use-package eglot
  :defer t
  :hook
  ((css-mode css-ts-mode
             ruby-mode ruby-ts-mode
             python-mode python-ts-mode
             yaml-mode yaml-ts-mode
             html-mode html-ts-mode
             js-mode js-ts-mode
             json-mode json-ts-mode
             scss-mode scss-ts-mode)
   . eglot-ensure)

  ((eglot-managed-mode . juank/eglot-eldoc)
   (eglot-managed-mode . juank/eglot-capf)
   )
  :preface
  (defun juank/eglot-eldoc ()
    ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '((ruby-mode ruby-ts-mode)  "ruby-lsp"))
  ;;(setq eglot-events-buffer-size 0)
  ;;(add-to-list 'eglot-stay-out-of 'flycheck)
  ;;(setq completion-category-overrides '((eglot (styles orderless))))
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                                                   :plugins (
                                                             :pycodestyle (:enabled :json-false)
                                                             :mccabe (:enabled :json-false)
                                                             :pyflakes (:enabled :json-false)
                                                             :flake8 (:enabled :json-false
                                                                               :maxLineLength 88)
                                                             :ruff (:enabled t
                                                                             :lineLength 88)
                                                             :pydocstyle (:enabled t
                                                                                   :convention "numpy")
                                                             :yapf (:enabled :json-false)
                                                             :autopep8 (:enabled :json-false)
                                                             :black (:enabled t
                                                                              :line_length 88
                                                                              :cache_config t))))))
  (defun juank/eglot-capf ()
    ;; I don't want `eglot-completion-at-point' to trample my other completion
    ;; options.
    ;;
    ;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'juank/version-control/project-capf
                       #'juank/version-control/issue-capf
                       #'eglot-completion-at-point

                       #'cape-keyword))))
  )

(use-package eldoc
  ;; Helps with rendering documentation
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package s
  ;; The long lost Emacs string manipulation library.
  ;; (See https://github.com/magnars/s.el/)
  )

(use-package which-key
  ;; optional if you want which-key integration  
  :config
  (which-key-mode))

(use-package treemacs

  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
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
    (treemacs-hide-gitignored-files-mode nil)
    )

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
  )

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  )

(use-package treemacs-nerd-icons)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package diff-hl
  :hook
  (prog-mode-hook . diff-hl-mode))

(use-package string-inflection
  :config
  ;; default
  (global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)

  ;; for ruby
  (add-hook 'ruby-mode-hook
            #'(lambda ()
                (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))

  ;; for elixir
  (add-hook 'elixir-mode-hook
            #'(lambda ()
                (local-set-key (kbd "C-c C-u") 'string-inflection-elixir-style-cycle)))

  ;; for java
  (add-hook 'java-mode-hook
            #'(lambda ()
                (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))

  ;; for python
  (add-hook 'python-mode-hook
            #'(lambda ()
                (local-set-key (kbd "C-c C-u") 'string-inflection-python-style-cycle)))
  )

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode nil)
  (setq flycheck-highlighting-mode 'lines)
  )

(use-package ag)

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (yas-reload-all)
  (add-hook 'sh-mode #'yas-minor-mode)
  (add-hook 'c-mode-hook #'yas-minor-mode)
  (add-hook 'java-mode-hook #'yas-minor-mode)
  (add-hook 'rust-mode-hook #'yas-minor-mode)
  (add-hook 'ruby-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  (add-hook 'php-mode-hook #'yas-minor-mode)
  (add-hook 'javascript-mode-hook #'yas-minor-mode)
  (add-hook 'typescript-mode-hook #'yas-minor-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode)
  )

(use-package yasnippet-snippets)

(use-package envrc
  :config
  (envrc-global-mode)
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'text-mode-hook 'my-display-numbers-hook)
;;(global-display-line-numbers-mode 1)
(global-visual-line-mode -1)
(global-hl-line-mode -1)

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
(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

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
;; truncate, truncate truncate!
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

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Automatically remove trailing whitespace.
(add-hook 'prog-mode-hook
          (lambda()
            (unless (derived-mode-p 'markdown-mode)
              (add-hook 'before-save-hook
                        #'cleanup-buffer t t))))

;; ignore bell alarm completely
(setq ring-bell-function 'ignore)

;; Always start Emacs with a split view
(split-window-horizontally)

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

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Enable `erase-buffer' function
(put 'erase-buffer 'disabled nil)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

;; Smooth Scrolling
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 10)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; ;; Horizontal  Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; Titlebar
(setq-default frame-title-format '("Emacs - " user-login-name "@" system-name " - %b"))

(when (version<= "29.1" emacs-version)
  (pixel-scroll-precision-mode 1))

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
;; redefine the isearch-forward-regexp function
(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

(use-package hl-todo
  :config (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#7f0000") ;; red-intense
          ("STUDY"  . "#ffa500")
          ("IMPORTANT"   . "#ffff00")
          ("NOTE"   . "#00ff00")
          ("FIXME"  . "#ffff00")))) ;; red-intense

;; Better Compilation
;; kill compilation process before starting another
(setq-default compilation-always-kill t)
;; save all buffers on `compile'
(setq-default compilation-ask-about-save nil)
;; Get compilation buffer to autoscroll. Always!!!
(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; setup compile package
(require 'compile)
(setq mode-compile-always-save-buffer-p nil)

(use-package vterm
  :disabled
  :config
  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))

  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)
  )

;; Debugger
(use-package dap-mode

  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

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
;;(global-set-key (kbd "C-<return>") 'company-complete)
;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c C-w") 'whitespace-mode)
;; Save all buffers
(global-set-key (kbd "C-x C-a") #'save-all-buffers)

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

(global-set-key "\C-s" 'consult-line)
                                        ;(global-set-key "\C-r" 'swiper-backward)
                                        ;(global-set-key (kbd "M-x") 'counsel-M-x)
                                        ;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
                                        ;(global-set-key (kbd "C-x C-p") 'counsel-projectile)
                                        ;(global-set-key (kbd "C-c C-r") 'ivy-resume)
                                        ;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;;(global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-c C-a" 'org-agenda)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; Comment and uncommpent shortcuts
(global-set-key  (kbd "C-c c") 'comment-region)
(global-set-key  (kbd "C-c u") 'uncomment-region)

;;(global-set-key (kbd "C-x M-t") 'cleanup-region)
;;(global-set-key (kbd "C-c n") 'cleanup-buffer)

;;(global-set-key (kbd "M-m") 'make-without-asking )
;;(global-set-key (kbd "M-n") 'clean-without-asking)
;;(global-set-key (kbd "C-c C-r") 'run-without-asking)
;;(define-key global-map "\M-m" 'make-without-asking)
;;(define-key global-map "\C-m c" )

(global-set-key (kbd "C-c i") 'string-inflection-cycle)
;; Force to CamelCase
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)
;; Force to lowerCamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)
;; Cycle through Java styles
(global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle)
;; Cycle through underscor
(global-set-key (kbd "C-c _") 'string-inflection-underscore)

(setq split-window-preferred-function 'juank-never-split-a-window)


(defun juank-align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

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

;; custom grep tool
(defun my-grep ()
  "grep recursively for something.  defaults to item at cursor
                              position and current directory."
  (interactive)
  (grep (read-string "run grep as: " (concat "grep -isrni " "\"" (thing-at-point 'symbol) "\"" " .")))
  )

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
                                        ;(indent-buffer)
  ;;(untabify-buffer)
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

(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
  (interactive)
  (save-some-buffers t))


(defun edit-configs ()
  "Opens the README.org file."
  (interactive)
  (find-file "~/.emacs.d/config.org"))

(global-set-key (kbd "C-z e") #'edit-configs)



(defun save-and-update-includes ()
  "Update the line numbers of #+INCLUDE:s in current buffer.
              Only looks at INCLUDEs that have either :range-begin or :range-end.
              This function does nothing if not in `org-mode', so you can safely
              add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))

(add-hook 'before-save-hook #'save-and-update-includes)

(defun decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
              BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" (+ l 1) (- r 1)))))) ;; Exclude wrapper


(defun where-am-i ()
  "An interactive function showing function `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (if (buffer-file-name) (buffer-file-name) (buffer-name)))))

(use-package speed-type
  :commands (speed-type-text))

(use-package tetris
  :ensure nil
  :commands (tetris)
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-n" . tetris-rotate-down)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-SPC" . tetris-move-bottom))
  :config
  (defadvice tetris-end-game (around zap-scores activate)
    (save-window-excursion ad-do-it)))

(use-package 2048-game
  :commands (2048-game))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . juank/prog-mode-configurator)
  :init
  (defun juank/prog-mode-configurator ()
    "Do the configuration of all the things."
    (setq truncate-lines t)
    ))

;; Commenting this as It seems to have issues with Emacs on other platforms
;; STUDY: Need to investifate further
;; (use-package turbo-log
;;   ;; Debug by logging on steroids
;;   :quelpa (turbo-log :fetcher github :repo "artawower/turbo-log.el")
;;   :bind (("C-s-l" . turbo-log-print)
;;          ("C-s-i" . turbo-log-print-immediately)
;;          ("C-s-h" . turbo-log-comment-all-logs)
;;          ("C-s-s" . turbo-log-uncomment-all-logs)
;;          ("C-s-[" . turbo-log-paste-as-logger)
;;          ("C-s-]" . turbo-log-paste-as-logger-immediately)
;;          ("C-s-d" . turbo-log-delete-all-logs))
;;   :config
;;   (setq turbo-log-msg-format-template "\"🚀: %s\"")
;;   (setq turbo-log-allow-insert-without-tree-sitter-p t))

(use-package devdocs
  ;; Download and install documents from https://devdocs.io/
  ;; Useful for having local inline docs.  Perhaps not always in the format that
  ;; I want, but can't have everything.
  )

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "GemFile\\'"
  :bind
  (:map ruby-mode-map (("C-M-h" . juank/treesit/function-select)
                       ("C-c y f" . juank/treesit/yank-qualified-method-fname)
                       ))
  :hook
  ((ruby-mode ruby-ts-mode) .
   (lambda ()
     (eldoc-mode)
     (setq fill-column 100)))
  (ruby-ts-mode . subword-mode)
  (ruby-mode . subword-mode)
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil)
  )

(use-package rspec-mode
  ;; For ruby I write my tests using rspec. This tool helps managing tests
  :after ruby-mode
  :config
  ;;(rspec-docker-container "web")
  ;;(rspec-use-spring-when-possible t)
  ;;(rspec-use-docker-when-possible t)
  ;;(rspec-docker-cwd "./")
  ;;(rspec-docker-command "docker compose exec")
  ;;(rspec-install-snippets)
  :hook ((dired-mode . rspec-dired-mode)
         (ruby-mode . rspec-mode)
         (ruby-ts-mode . rspec-mode))
  ;; Dear reader, make sure that you can jump from spec and definition.  And in
  ;; Ruby land when you have lib/my_file.rb, the corresponding spec should be in
  ;; spec/my_file_spec.rb; and when you have app/models/my_file.rb, the spec
  ;; should be in spec/models/my_file_spec.rb
  :bind (:map rspec-mode-map (("C-c r s" . 'rspec-toggle-spec-and-target)))
  :bind (:map ruby-mode-map (("C-c r s" . 'rspec-toggle-spec-and-target)))
  :init
  (defun juank/ruby/rspec-spring-p ()
    "Check the project for spring as part of the Gemfile.lock."
    (let ((gemfile-lock (f-join (projectile-project-root) "Gemfile.lock")))
      (and (f-exists? gemfile-lock)
           (s-present?
            (shell-command-to-string
             (concat "rg \"^ +spring-commands-rspec \" " gemfile-lock))))))
  ;; Out of the box, for my typical docker ecosystem, the `rspec-spring-p'
  ;; function does not work.  So I'm overriding the default behavior to match my
  ;; ecosystem.
  (advice-add #'rspec-spring-p :override #'juank/ruby/rspec-spring-p))

(use-package inf-ruby
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter-and-focus)
  (add-hook 'ruby-base-mode 'inf-ruby-minor-mode)
  (inf-ruby-enable-auto-breakpoint))

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

(defun juank_ruby_require_debugger ()
  "determine the correct debugger based on the gemfile"
  (let ((gemfile_lock (f_join (projectile_project_root) "gemfile_lock")))
    (if_let* ((f_exists? gemfile_lock)
              (debuggers
               (s_split "\n"
                        (shell_command_to_string
                         (concat
                          "rg \"^ +(byebug|debugger|pry_byebug|debug) \" "
                          gemfile_lock
                          " r '$1' only_matching | uniq")))))
             (cond
              ((member "byebug" debuggers) "require 'byebug'; byebug")
              ((member "debug" debuggers) "require 'debug'; binding_break")
              ((member "debugger" debuggers) "require 'debugger'; debugger")
              ((member "pry_byebug" debuggers) "require 'pry_byebug'; binding_pry")
              (t "require 'debug'; binding_break"))
             "require 'debug'; binding_break")))

;;(use-package yard-mode
;; Preferred Ruby documentation syntax
;;:hook ((ruby-mode ruby-ts-mode ) . yard-mode)
;;:hook ((ruby-mode ruby-ts-mode) . eldoc-mode))


(defun juank/ruby-ts-mode-configurator ()
  "Configure the `treesit' provided `ruby-ts-mode'."
  ;; I encountered some loading issues where ruby-ts-mode was not available
  ;; during my understanding of the use-package life-cycle.
  (setq-local add-log-current-defun-function #'juank/treesit/yank-qualified-method-fname)
  (define-key ruby-ts-mode-map (kbd "C-M-h") #'juank/treesit/function-select)

  (add-hook 'ruby-ts-mode-hook #'juank/ruby-ts-mode-configurator))

(defun my-python-repl-command ()
  (interactive)
  (call-interactively 'pipenv-activate)
  (call-interactively 'run-python)
  )



(defun my-python-eglot-hook ()
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'python-mode #'flyspell-prog-mode)
  (add-hook 'python-mode #'superword-mode)
  (add-hook 'python-mode #'hs-minor-mode)
  (add-hook 'python-mode #'(lambda () (set-fill-column 88)))
  (add-hook 'python-ts-mode #'eglot-ensure)
  (add-hook 'python-ts-mode #'flyspell-prog-mode)
  (add-hook 'python-ts-mode #'superword-mode)
  (add-hook 'python-ts-mode #'hs-minor-mode)
  (add-hook 'python-ts-mode #'(lambda () (set-fill-column 88)))
  (add-hook 'python-mode-hook
            (lambda () (local-set-key (kbd "<f7>") #'my-python-repl-command)))
  )


(use-package python

  :defer t
  :config
  (my-python-eglot-hook)
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

;; ;; ;; Hide the modeline for inferior python processes
;; (use-package inferior-python-mode
;;   :ensure nil
;;   :hook (inferior-python-mode . hide-mode-line-mode))


;; ;; Required to hide the modeline
;; (use-package hide-mode-line
;;   :defer t)

;; ;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located. I use miniconda.
(use-package pyvenv

  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/.local/share/virtualenvs/"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

(use-package pipenv

  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; Buffer formatting on save
(use-package blacken

  :defer t
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  :hook (python-mode . blacken-mode))


;; numpy docstring for python
(use-package numpydoc

  :defer t
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))


;; Format the python buffer following YAPF rules
;; There's also blacken if you like it better.
;; (use-package yapfify
;;   
;;   :defer t
;;   :hook (python-mode . yapf-mode))

(use-package prettier-js)

(use-package add-node-modules-path)

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  (add-node-modules-path)
  (prettier-js-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html.erb?$" . web-mode)) ;; auto-enable for rails' .erb files
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  ;; Enable eslint checker for web-mode
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; Enable flycheck globally
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :hook ((web-mode-hook . web-mode-init-hook))
  :after (prettier-js-mode add-node-modules-path )
  )

;; if you use treesitter based typescript-ts-mode (emacs 29+)
(use-package tide
  :after (flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package typescript-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-node-modules-path)
  (prettier-js-mode)
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  )

(use-package php-mode)

(use-package dockerfile-mode
  :defer t
  )

(use-package conf-mode)

(use-package markdown-mode
  :config
  (setq markdown-command "/usr/bin/markdown")
  (setq markdown-css-paths `(,(expand-file-name "Documents/markdown.css")))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  )

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  )

(use-package sly)

(use-package rust-mode)

(use-package kotlin-mode)

(use-package go-mode)

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

(use-package haskell-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode)))

(use-package rustic
  :config
  (setq rustic-format-trigger 'on-save)
  (setq rustic-format-on-save-method 'rustic-format-buffer)
  (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode)))
