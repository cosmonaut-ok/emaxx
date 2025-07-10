;;; projectile.el --- Projectile configuration for Emaxx -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package projectile
  :diminish
  :bind
  (:map projectile-command-map
        ("." . #'helm-projectile-find-file-dwim))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (custom-set-variables '(projectile-enable-caching t))
  (global-set-key (kbd "<f9>") 'projectile-run-project)
  (global-set-key (kbd "<f8>") 'projectile-configure-project)
  (global-set-key (kbd "<f7>") 'projectile-bootstrap-project)

  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "cmake.bld")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "bld")
  (add-to-list 'projectile-globally-ignored-directories "cmake-build")
  (add-to-list 'projectile-globally-ignored-directories "debian")
  (add-to-list 'projectile-globally-ignored-directories ".deps")
  (add-to-list 'projectile-globally-ignored-directories ".venv")

  ;; TODO: The following feature seems to has changed and a new solution needs to
  ;; be developed.  Probably involving setting up `projectile-mode-line-function'
  ;; to something based on `projectile-default-mode-line'.
  ;; ;; Colorize the name of the current project in the modeline.
  ;; (defface exordium-project-name '((t (:inherit mode-line)))
  ;;   "Face for the name of the current project in the modeline."
  ;;   :group 'exordium)
  ;; (setq projectile-mode-line
  ;;       `(:eval (if (file-remote-p default-directory)
  ;;                   (list " ["
  ;;                         (propertize "*remote*"
  ;;                                     'face 'exordium-project-name)
  ;;                         "]")
  ;;                 (list " ["
  ;;                       (propertize (projectile-project-name)
  ;;                                   'face 'exordium-project-name)
  ;;                       "]"))))
  )
;;; projectile.el ends here.
