;;; ansible.el --- ansible-mode configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ansible
  :init
  (defun ansible-init()
    (lsp)
    (lsp-ui-mode t)
    (ansible-doc-mode t))
  :ensure t
  :hook (ansible-mode . ansible-init)
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package poly-ansible
  :ensure t
  :after ansible)

(use-package ansible-doc
  :ensure t
  :after ansible
  :hook (ansible-mode . ansible-doc-mode))

;;; ansible.el ends here.
