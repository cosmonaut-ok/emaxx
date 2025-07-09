
;;
;; helm section. See helm usage here: https://github.com/emacs-helm/helm/ for details
;;
(use-package helm
	     ;; :straight t
	     ;; :after (require 'helm-xref)
	     :config
	     ;; quick and dirty hack to make helm-M-x working
	     ;; TODO: remove in the future
	     (load-library "helm-lib.el")
	     :bind (("M-x" . helm-M-x)
		    ("C-x C-f" . helm-find-files)
		    ("C-x b" . helm-mini)
		    ("C-x C-b" . helm-mini)
		    ([f10] . helm-buffers-list)
		    ([S-f10] . helm-recentf)))

(use-package helm-xref
	     :requires helm)

(use-package helm-icons
	     :requires helm
	     :config (helm-icons-enable))
