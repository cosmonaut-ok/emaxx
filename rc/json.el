;;; json.el --- json-mode configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package json-mode
  :ensure t
  :init
  (defun init-json()
    (lsp)
    (lsp-ui-mode t)
    (yas-minor-mode t)
    (projectile-mode t)
    (format-all-mode t))
  :hook (json-mode . init-json)
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )
;;; json.el ends here.
