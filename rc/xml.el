;;; xml.el --- yaml-mode configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package nxml-mode
  :init
  (defun init-xml()
    (lsp)
    (lsp-ui-mode t))
  :hook (nxml-mode . init-xml)
  :custom
  (nxml-slash-auto-complete-flag t))
;;; xml.el ends here.
