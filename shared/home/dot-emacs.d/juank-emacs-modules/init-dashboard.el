;;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-funcs))

(use-package dashboard
  :diminish dashboard-mode
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("R" . restore-session)
         ("S" . find-custom-file)
         ("U" . update-config-and-packages)
         ("q" . quit-dashboard))

  :hook (dashboard-mode . (lambda ()
                            ;; No title
                            (setq-local frame-title-format nil)
                            ;; Enable `page-break-lines-mode'
                            (when (fboundp 'page-break-lines-mode)
                              (page-break-lines-mode 1))))



  :init
  (setq dashboard-banner-logo-title "Welcome Master. What are we working on today?"

        dashboard-startup-banner (juank-dashboard-logo)
        ;;dashboard-page-separator "\n\f\n"
        dashboard-projects-backend 'project-el
        dashboard-path-max-length 60
        dashboard-center-content t
        dashboard-show-shortcuts t 
        dashboard-display-icons-p #'icons-displayable-p ;; t if this evaluation doesnt work
        dashboard-icon-type 'nerd-icons 
        dashboard-set-init-info t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t

        ;; Add naviator buttons here

        dashboard-set-week-agenda t
        dashboard-week-agenda t
        dashboard-heading-icons '((recents   . "nf-oct-history")
                                  (bookmarks . "nf-oct-bookmark")
                                  (agenda    . "nf-oct-calendar")
                                  (projects  . "nf-oct-rocket")
                                  (registers . "nf-oct-database")
                                  )
        dashboard-week-agenda t
        dashboard-items '((recents . 10)
                          (agenda . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)
                          )

        dashboard-set-footer t
        dashboard-footer-icon
        (if (icons-displayable-p)
            (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)
          (propertize ">" 'face 'dashboard-footer))
        )
  (dashboard-setup-startup-hook)

  :config
  (defun my-dashboard-insert-copyright ()
    "Insert copyright in the footer."
    (when dashboard-set-footer
      (dashboard-insert-center
       (propertize (format "\nPowered by Juank Prada and the OSS comunity, %s\n" (format-time-string "%Y"))
                   'face 'font-lock-comment-face))))
  (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)

  (defun restore-session ()
    "Restore the previous session."
    (interactive)
    (message "Restoring previous session...")
    (quit-window t)
    (cond
     ((bound-and-true-p tabspaces-mode)
      (tabspaces-restore-session))
     ((bound-and-true-p desktop-save-mode)
      (desktop-read)))
    (message "Restoring previous session...done"))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (let ((func (local-key-binding "p")))
      (and func (funcall func))))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (let ((func (local-key-binding "m")))
      (and func (funcall func))))

  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (length> (window-list-1)
                 ;; exclude `treemacs' window
                 (if (and (fboundp 'treemacs-current-visibility)
                          (eq (treemacs-current-visibility) 'visible))
                     2
                   1))
        (setq dashboard-recover-layout-p t))

    ;; Display dashboard in maximized window
    (delete-other-windows)

    ;; Refresh dashboard buffer
    (dashboard-refresh-buffer)

    ;; Jump to the first section
    (dashboard-goto-recent-files))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (and dashboard-recover-layout-p
         (and (bound-and-true-p winner-mode) (winner-undo))
         (setq dashboard-recover-layout-p nil))))




(provide 'init-dashboard)
