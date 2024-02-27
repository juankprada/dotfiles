;; init-ui.el --- Make this thing look good.	-*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)


;; Initial frame
;; start full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq initial-frame-alist '((top . 0.5)
                            (left . 0.5)
                            (width . 0.628)
                            (height . 0.8)
                            (fullscreen)))


;; Title
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)


(when (and sys/mac-ns-p sys/mac-x-p)
  ;;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;;(add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (if (display-graphic-p)
                  (menu-bar-mode 1)
                (menu-bar-mode -1))))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))


;; Always start Emacs with a split view
;;(split-window-horizontally)

(use-package modus-themes
  :init
  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi-tinted)
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-disable-other-themes t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)



  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package fontaine
  :hook (after-init . juank-fontain-init-hook)
  ;; A narrow focus package for naming font configurations and then selecting
  ;; them.
  :config
  (defun juank-fontain-init-hook()
    (fontaine-set-preset 'default))
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
           :line-spacing nil))))

;; Mode-line
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
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
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  :bind (:map doom-modeline-mode-map
              ("C-<f6>" . doom-modeline-hydra/body))
  )


;; A minor-mode menu for mode-line
(use-package minions

  :hook (doom-modeline-mode . minions-mode))

(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           embark-collect-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
         (dired-mode . (lambda()
                         (and (bound-and-true-p hide-mode-line-mode)
                              (turn-off-hide-mode-line-mode))))))

;; TODO: Check how to achieve this. There is an error wwith `font-installed-p` not being defined
;; Icons
;; (use-package nerd-icons
;;   :config
;;   (when (and (display-graphic-p)
;;              (not (font-installed-p nerd-icons-font-family)))
;;       (nerd-icons-install-fonts t)))

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )
;; (use-package nerd-icons-completion
;;   ;;:after marginalia
;;   :config
;;   (nerd-icons-completion-mode))
;;   (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package display-line-numbers
  :ensure nil
  :defer t
  :config
  (defun juank-display-line-numbers-hook ()
    (setq display-line-numbers-type 'relative)
    (display-line-numbers-mode 1)
    )
  :hook ((prog-mode yaml-mode conf-mode) . juank-display-line-numbers-hook)
  :init (setq display-line-numbers-width-start t))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)



;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 10
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Horizontal  Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)



;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (unless sys/macp
    (use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))))

;; Smooth scrolling over images
(unless emacs/>=30p
  (use-package iscroll
    :diminish
    :hook (image-mode . iscroll-mode)))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;; Child frame

(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))


;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; ignore bell alarm completely
(setq ring-bell-function 'ignore)

(provide 'init-ui)
