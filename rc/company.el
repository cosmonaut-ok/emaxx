;;; company.el --- company (autocompletion package) configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package company
  :commands (company-other-backend)
  :init (defun company-init()
          (company-mode)
          (company-quickhelp-mode)
          (local-set-key (kbd "C-<tab>") 'company-complete))

  :custom
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-files-exclusions '(".git/" ".gitignore" ".gitmodules" ".DS_Store"
                              ".vscode/" ".envrc" ".direnv/" ".clangd"
                              "venv/" ".venv/"))
  (company-transformers '(delete-consecutive-dups
                          company-sort-by-occurrence))
  :config
  (add-to-list 'company-backends
               '(company-capf company-yasnippet company-files
                              :with company-dabbrev-code))
  :hook (prog-mode . company-init)
  :bind
  (("C-." . #'company-complete)
   :map company-active-map
   ("C-\\" . #'company-other-backend)
   ("C-o" . #'company-other-backend)))

(use-package company-quickhelp
  :ensure t
  :after company
  :custom
  (company-quickhelp-color-foreground "turquoise")
  (company-quickhelp-color-background "gray26")
  )

(use-package company-quickhelp-terminal
  :ensure t
  :after company-quickhelp
  :hook (company-quickhelp . company-quickhelp-terminal-mode))

(use-package company-statistics
  :after company
  :config
  (company-statistics-mode)
  (add-to-list 'company-transformers
               'company-sort-by-backend-importance 'append))

(use-package company-posframe
  :init
  (use-package company :commands (company-show-doc-buffer))
  :commands (company-posframe-quickhelp-toggle
             company-posframe-quickhelp-scroll-down
             company-posframe-quickhelp-scroll-up)
  :bind
  (:map company-posframe-active-map
        ("C-h" . #'company-posframe-quickhelp-toggle)
        ("C-M-h" . #'company-show-doc-buffer)
        ("C-S-v" . #'company-posframe-quickhelp-scroll-up)
        ("M-V" . #'company-posframe-quickhelp-scroll-down))
  :custom
  (company-posframe-quickhelp-delay 0.2)
  (company-posframe-quickhelp-x-offset 5)
  :config
  (company-posframe-mode 1))

(use-package company-math
  :ensure t
  :after company
  :config
  ;; global activation of the unicode symbol completion
  (add-to-list 'company-backends 'company-math-symbols-unicode))
;;; company.el ends here.
