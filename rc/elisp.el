;;; elisp.el --- Configuration for Emacs Lisp   -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- ---------------------------------------------------------
;; Key               Definition
;; ----------------- ---------------------------------------------------------
;; M-C-g             `helm-imenu' (lists functions and variables in buffer)
;; C-c M-e           `pp-eval-last-sexp'

;;; Code:
(use-package helm :ensure nil)

(use-package elisp-mode
  :ensure nil
  :defer t
  :functions (emaxx--pp-last-sexp-filter-quote)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :init
  (defun emaxx--pp-output-setup ()
    "Enable `lexical-binding' and disable `flycheck-checkdoc' in PP output buffers."
    (when (string-match-p (rx string-start
                              "*Pp " (or "Eval" "Macroexpand") " Output*"
                              string-end)
                          (buffer-name))
      (when (boundp 'flycheck-disabled-checkers)
        (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))
      (goto-char (point-min))
      (insert ";; -*- lexical-binding: t -*-\n\n")
      (goto-char (point-max))
      (setq-local lexical-binding t)))

  :bind
  (:map emacs-lisp-mode-map
        ("M-C-g" . #'helm-imenu)
        ("C-c M-e" . #'pp-eval-last-sexp)
        ("C-c C-M-e" . #'pp-macroexpand-last-sexp)
        ("M-." . #'xref-find-definitions)
        ("M-," . #'xref-go-back)
        ("M-r" . #'xref-find-references)
        ("M-?" . #'helpful-at-point)
        ("C-c C-d" . #'helpful-at-point)
        :map lisp-interaction-mode-map
        ("M-C-g" . #'helm-imenu)
        ("C-c M-e" . #'pp-eval-last-sexp)
        ("C-c C-M-e" . #'pp-macroexpand-last-sexp)
        ("M-." . #'xref-find-definitions)
        ("M-," . #'xref-go-back)
        ("M-r" . #'xref-find-references)
        ("M-?" . #'helpful-at-point)
        ("C-c C-d" . #'helpful-at-point))
  :hook
  (emacs-lisp-mode . emaxx--pp-output-setup))
;;; elisp.el ends here
