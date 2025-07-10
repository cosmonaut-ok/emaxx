;;; terraform.el --- ansible-mode configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package terraform-mode
  ;; if using straight
  ;; :straight t

  ;; if using package.el
  :ensure t
  :custom (terraform-indent-level 4)
  :init
  (defun terraform-init ()
    ;; if you want to use outline-minor-mode
    (outline-minor-mode t))
  :hook (terraform-mode . terraform-init)
  :config
  (company-terraform-init))

(use-package company-terraform
  :ensure t
  :after terraform
  :config
  (add-to-list 'company-backends 'company-terraform))
;;; terraform.el ends here.
