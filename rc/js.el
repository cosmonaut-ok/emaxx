;;; js.el --- various web modes configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:

;; Enable tree-sitter global mode
;; (use-package tree-sitter :ensure t)
;; (use-package tree-sitter-langs :ensure t)

;; (global-treesit-mode 1)

;; ;; Remap old typescript-mode to the new tree-sitter based modes
;; (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode)) ; Also good for plain JSX files

;;; Code:

;; js2-mode
(use-package js2-mode
  ;; ;; Open JSON files with j2 modes by default, instead of the built-in
  ;; ;; javascript mode.
  ;; :mode "\\.json'"
  :hook (js . js-minor-mode)
  :custom
  ;; Define some RDEL symbols
  (js2-global-externs (mapcar 'symbol-name '(BB debug assert activebase)))
  :bind
  (:map js-mode-map
        ("C-M-g" . #'helm-imenu))
  :config
  ;; js2-mode provides 4 level of syntax highlighting. They are:
  ;; - 0 or a negative value means none.
  ;; - 1 adds basic syntax highlighting.
  ;; - 2 adds highlighting of some Ecma built-in properties.
  ;; - 3 adds highlighting of many Ecma built-in functions.
  (setq js2-highlight-level 3)
  ;; (js2-imenu-extras-mode)
  )

(setq poly-lock-allow-fontification nil)
(setq poly-lock-allow-background-adjustment nil)

(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode t)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode t)
    (tide-hl-identifier-mode t)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  :custom
  ;; aligns annotation to the right hand side
  (company-tooltip-align-annotations t)

  :config
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  ;; if you use typescript-mode
  ;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
  ;; if you use treesitter based typescript-ts-mode (emacs 29+)
  (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
  )

(use-package vue3-mode :ensure t)
(use-package vue-html-mode :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package company-web
  :ensure t
  :init
  (defun company-web-init ()
    (set (make-local-variable 'company-backends) '(company-web-html)))
  :after (web-mode company)
  :config
  (define-key web-mode-map (kbd "C-'") 'company-web-html)
  :hook (web-mode . company-web-init))
;;; js.el ends here.
