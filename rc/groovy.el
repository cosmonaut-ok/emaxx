;;; groovy.el --- groovy-mode configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package groovy-mode
  :ensure t
  :config
  ;; we are not using LSP as
  (require 'inf-groovy)
  :custom
  (groovy-indent-offset 4))

(use-package jenkinsfile-mode
  :after groovy-mode
  :hook (jenkinsfile-mode . groovy-electric-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.jdp\\'" . jenkinsfile-mode))
  (add-to-list 'auto-mode-alist '("\\.jenkinsfile\\'" . jenkinsfile-mode))
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . jenkinsfile-mode))
  (add-to-list 'auto-mode-alist '("Jenkinsfile\\." . jenkinsfile-mode)))
;;; groovy.el ends here.
