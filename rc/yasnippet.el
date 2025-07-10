;;; yasnippet.el --- yasnippet configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package yasnippet
  :ensure t
  :commands (yas-expand-from-trigger-key yas-load-directory)
  ;; Enable YAS only for C and C++
  :hook ((c-mode-common . yas-minor-mode))
  :bind
  (:map yas-minor-mode-map
        ("C-c y" . #'yas-expand-from-trigger-key))
  :config
  ;; Directory tree where to find snippets (subdirectories must be mode names).
  ;; The t means JIT loading, which saves time during Emacs startup.
  (yas-load-directory (locate-user-emacs-file "snippets") t))

;;   :config (yas-global-mode))

;;; yasnippet.el ends here.
