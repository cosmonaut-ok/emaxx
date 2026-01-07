;;; treemacs.el --- treemacs configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; :custom and :init->custom-set-variables
;; are not applicable exactly for treemacs
;; for some unknown reason. Setting required variables
;; just in config-wide environment
(custom-set-variables
 '(treemacs-display-in-side-window t)
 ;; '(treemacs-indent-guide-mode nil)
 ;; '(treemacs-indentation-string " │")
 '(treemacs-is-never-other-window t)
 '(treemacs-silent-refresh t)
 '(treemacs-sorting 'alphabetic-case-insensitive-asc)
 '(treemacs-width 40)
 '(treemacs-width-increment 4)
 '(treemacs-width-is-initially-locked nil)
 '(treemacs-workspace-switch-cleanup 'all)
 '(treemacs-text-scale -0.8)
 '(treemacs-show-hidden-files t)
 '(treemacs-litter-directories '("/node_modules" "/.venv" "/.cask"))
 '(treemacs-show-cursor nil)
 '(treemacs-filewatch-mode t)
 '(treemacs-silent-filewatch t)
 '(treemacs-file-event-delay 2000)
 '(treemacs-file-follow-delay 0.2)
 '(treemacs-recenter-after-file-follow 'always)
 '(treemacs-recenter-after-project-expand 'always)
 '(treemacs-recenter-after-project-jump 'always)
 '(treemacs-recenter-after-tag-follow nil)
 '(treemacs-eldoc-display 'detailed)
 '(treemacs-follow-after-init t)
 '(treemacs-expand-after-init t)
 '(treemacs-find-workspace-method 'find-for-file-or-pick-first)

 ;; '(treemacs-fringe-indicator-mode 'always)
 ;; '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

 '(treemacs-deferred-git-apply-delay 0.5)
 '(treemacs-goto-tag-strategy 'refetch-index)
 '(treemacs-hide-dot-git-directory t)
 '(treemacs-max-git-entries 5000)

 '(treemacs-error-list-expand-depth 2)
 '(treemacs-error-list-current-project-only t)

 '(treemacs-tag-follow-cleanup t)
 '(treemacs-tag-follow-delay 0.5)
 '(treemacs-project-follow-into-home nil)

;;  ;; '(treemacs-header-scroll-indicators '(nil . "^^^^^^"))

 '(treemacs-position 'left)
 '(treemacs-read-string-input 'from-child-frame)
 )

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (if treemacs-python-executable
      (progn
        (setq treemacs-collapse-dirs 3)
        (treemacs-git-commit-diff-mode t))
    (setq treemacs-collapse-dirs 0))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  ;; (lsp-treemacs-errors-list)
  ;; (lsp-treemacs-symbols)
  ;; ;; (treemacs-select-window)

  ;; ;; exclude lsp-treemacs error list and symbols
  ;; ;; from other-window command
  ;; (dolist (w (window-list))
  ;;   (let ((buf-name (buffer-name (window-buffer w))))
  ;;     (when (or
  ;;       (equal "*LSP Error List*" buf-name)
  ;;       (equal "*LSP Symbols List*" buf-name))
  ;;  (set-window-parameter w 'no-other-window t))))
  )

(use-package treemacs
  ;; :hook (prog-mode . treemacs)
  :after lsp
  :config
  ;; (treemacs--setup-fringe-indicator-mode 'always)
  ;; ;; The default width and height of the icons is 22 pixels. If you are
  ;; ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;; (treemacs-resize-icons 16)
  :commands treemacs
  )

(use-package treemacs-all-the-icons
  :after (treemacs)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package lsp-treemacs
  :after (lsp treemacs)
  ;; :commands (lsp-treemacs-symbols lsp-treemacs-errors-list)
  )

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package treemacs-projectile
  :ensure t
  ;; :defer t
  :after (treemacs projectile)
  :init
  (defun treemacs-start ()
    (interactive)
    (let* ((buffer (current-buffer))
	   (window (and buffer (get-buffer-window buffer))))
      (treemacs)
      (lsp-treemacs-errors-list)
      (lsp-treemacs-symbols)
      (when window
        (select-window window)))

    ;; wait a bit while windows created
    ;; to exclude it from other-window
    (sleep-for 1)

    ;; exclude lsp-treemacs error list and symbols
    ;; from other-window command
    (dolist (w (window-list))
      (let ((buf-name (buffer-name (window-buffer w))))
        (when (or
	       (equal "*LSP Error List*" buf-name)
	       (equal "*LSP Symbols List*" buf-name))
	  (message "Setting no-other-window for %s" buf-name)
          (set-window-parameter w 'no-delete-other-windows t)
          (set-window-parameter w 'no-other-window t)))))
  :config
  (tool-bar-add-item "spell" 'treemacs-start
                     'treemacs-start
                     :help   "Launch treemacs")
  :bind
  (("C-c e" . #'treemacs)
   ("C-c E" . #'treemacs-projectile)))
;;; treemacs.el ends here.
