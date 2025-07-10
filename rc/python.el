;;; pytho.el --- pytho-related configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package conda
  :ensure t
  ;; if you want to automatically activate a conda environment on the opening of a file:
  :init (defun init-conda-file-open ()
          (when (bound-and-true-p conda-project-env-path)
            (conda-env-activate-for-buffer)))
  :hook (find-file . init-conda-file-open)
  :custom
  (conda-anaconda-home (expand-file-name "~/anaconda3/"))
  (conda-env-home-directory (expand-file-name "~/anaconda3/"))
  :config
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)

  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)

  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t)

  ;; add anaconda path to PATH env variable
  (add-to-list 'exec-path (concat conda-anaconda-home conda-env-executables-dir))
  (setenv "PATH" (concat conda-anaconda-home conda-env-executables-dir ":" (getenv "PATH")))
  )

(use-package flycheck-pyflakes
  :ensure t
  :after (flycheck python)
  :hook (python-mode . flycheck-mode)
  :config
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))

(use-package python
  :init
  (defun python-init()
    (flycheck-mode t)
    (lsp)
    (lsp-ui-mode t)
    (yas-minor-mode t)
    (projectile-mode t)
    (format-all-mode t))
  :after (conda lsp-mode flycheck)
  :ensure t
  :hook (python-mode . python-init)
  :custom
  (py-python-command (concat conda-anaconda-home conda-env-executables-dir "/python"))
  (py-ipython-command (concat conda-anaconda-home conda-env-executables-dir "/ipython"))
  (python-shell-exec-path py-python-command)
  )

(use-package py-snippets
  :ensure t
  :after yasnippet
  :config
  (py-snippets-initialize))

(use-package jinja2-mode :ensure t)

(use-package helm-pydoc
  :ensure t
  :after (helm python)
  :config
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc))
;;; python.el ends here.
