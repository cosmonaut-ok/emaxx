;;; k8s.el --- kubernetes-related configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

(use-package kubernetes-helm :ensure t)
;;; k8s.el ends here.
