;;; csharp.el --- csharp-mode configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package csharp-mode
  :init
  (defun csharp-mode-init ()
    (lsp)
    (lsp-ui-mode t))
  :ensure t
  :hook (csharp-mode . csharp-mode-init))
;;; csharp.el ends here.
