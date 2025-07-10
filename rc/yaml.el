;;; yaml.el --- yaml-mode configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package yaml-mode
  :ensure t
  :init
  (defun init-yaml()
    (lsp)
    (lsp-ui-mode t))
  :hook (yaml-mode . init-yaml)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )
;;; yaml.el ends here.
