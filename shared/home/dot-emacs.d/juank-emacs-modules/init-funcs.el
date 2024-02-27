;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-

;; Suppress warnings
(eval-when-compile
  (require 'init-const)
  )

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))

(defun juank-dashboard-logo ()
  "Returns the path to a random logo specifid in the `~/.emacs.d/logos` directory"
  (if (display-graphic-p)
      (concat "~/.emacs.d/logos/logo-" (number-to-string (random 21)) ".png")
    "banner.txt"))

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


(defun juank-previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
  )

(defun juank-next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
  )

(defun juank-byte-compile-dotfiles ()
  "Byte compile all Emacs dotfiles."
  (interactive)
  ;; Automatically recompile the entire .emacs.d directory.
  (byte-recompile-directory (expand-file-name config-dir) 0))

(defun juank-byte-compile-user-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    ;;(byte-compile-dotfiles)
    ;; (message "%s compiled" user-init-file)
    ))

;; Prevent C-x C-c to kill emacs!!
(defun juank-dont-kill-emacs()
  "Disable C-x C-c binding execute kill-emacs."
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))



;; function to call a command at a specific directory
(defun juank-at-directory-do ()
  "reads a directory name (using ido), then runs
                                    execute-extended-command with default-directory in the given
                                    directory."
  (interactive)
  (let ((default-directory
         (read-directory-name "in directory: "
                              nil nil t)))
    (call-interactively 'execute-extended-command)))


(defun juank-find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p juank-makescript) t
    (cd "../")
    (juank-find-project-directory-recursive)))


(defun juank-lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))


(defun juank-unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun juank-find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (juank-find-project-directory-recursive)
    (setq last-compilation-directory default-directory)))

(defun juank-make-without-asking ()
  "Make the current build."
  (interactive)
  (if (juank-find-project-directory) (compile juank-build-command))
  (other-window 1))


(defun juank-clean-without-asking()
  "Clean the current build."
  (interactive)
  (if (find-project-directory) (compile juank-clean-command))
  (other-window 1))

(defun juank-run-without-asking()
  "Run the current build."
  (interactive)
  (if (juank-find-project-directory) (compile juank-run-command))
  (other-window 1))


;; Function used to call the compile command at a specific dir
(defun juank-project-compile ()
  "reads a directory name then runs
                                    execute-extended-command with default-directory in the given
                                    directory."
  (interactive)
  (let ((default-directory
         (read-directory-name "compile in directory: "
                              nil nil t)))
    (call-interactively 'compile)))

;; custom grep tool
(defun juank-my-grep ()
  "grep recursively for something.  defaults to item at cursor
                                      position and current directory."
  (interactive)
  (grep (read-string "run grep as: " (concat "grep -isrni " "\"" (thing-at-point 'symbol) "\"" " .")))
  )

;; function to remove windows line ending
(defun juank-remove-windows-line-endings ()
  "removes the ^m line endings"
  (interactive)
  (replace-string "\^M" "")
  )


(defun juank-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun juank-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun juank-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
                                        ;(indent-buffer)
  ;;(untabify-buffer)
  (delete-trailing-whitespace))


(defun juank-cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))



;; search word at point
(defun juuank-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))


(defun juank-isearch-yank-word-hook ()
  (when (equal this-command 'juank-isearch-word-at-point)
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


(defun juank-to-unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun juank-to-dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun juank-to-mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))

;; function to duplicate current line
(defun juank-duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(defun juank-save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
  (interactive)
  (save-some-buffers t))


(defun juank-edit-configs ()
  "Opens the custom.org file."
  (interactive)
  (find-file "~/.emacs.d/config.org"))

(defun juank-save-and-update-includes ()
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


(defun juank-where-am-i ()
  "An interactive function showing function `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (if (buffer-file-name) (buffer-file-name) (buffer-name)))))

;; WORKAROUND: fix blank screen issue on macOS.
(defun juank-fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
    This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))

(defun update-packages ()
  "Refresh package contents and update all packages."
  (interactive)
  (message "Updating packages...")
  (package-upgrade-all)
  (message "Updating packages...done"))
(defalias 'juank-update-packages #'update-packages)


;; Fonts
(defun juank-install-fonts ()
  "Install necessary fonts."
  (interactive)
  (nerd-icons-install-fonts))

(provide 'init-funcs)
