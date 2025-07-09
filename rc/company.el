(use-package company
  :init (defun company-init()
	  (company-mode)
	  (company-quickhelp-mode)
	  (local-set-key (kbd "C-<tab>") 'company-complete))
  :hook (prog-mode . company-init))

(use-package company-quickhelp
  :after company)
