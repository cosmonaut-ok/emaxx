;;; autotools.el --- autotconf-mode and makefile-automake-mode configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst const/third-party-dir
  (expand-file-name (locate-user-emacs-file "3rd-party"))
  "Location of the third party directory.")

(add-to-list 'load-path const/third-party-dir)

(defun init-autotools()
  (lsp)
  (lsp-ui-mode))

(use-package autoconf-mode
  :hook (autoconf-mode . init-autotools))

(use-package makefile-automake-mode
  :hook (makefile-automake-mode . init-autotools))
;;; autotools.el ends here.
