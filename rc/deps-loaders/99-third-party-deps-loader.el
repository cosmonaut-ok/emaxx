;;; 99-third-party-deps-loader.el --- 3rd party dependencies loader for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;; This file is for loading emacs 3rd party packages,
;; defined in `deps.el` declarative file
;;; Code:
(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "package-deps-loader: Can not load deps file %s" emacs-user-deps)))

;;
;; install 3rd party packages
;;
(defun download-file (url download-path)
  (let ((download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      ;; we may have to trim the http response
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (write-file download-path))))

(defconst const/third-party-dir (expand-file-name (locate-user-emacs-file "3rd-party")))

;; create directory for third party scripts
(make-directory const/third-party-dir t)
(dolist (c const/third-party-packages-list)
  (when (not (file-exists-p (concat const/third-party-dir "/" (car c))))
    (download-file (cadr c) (concat const/third-party-dir "/" (car c)))))
