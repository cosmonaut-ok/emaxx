;; This file is for loading python`s pip packages,
;; defined in `deps.el` declarative file

(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "pip-deps-loader: Can not load deps file %s" emacs-user-deps)))

(defconst const/pip-cmd "pip")

(when (equal (shell-command-to-string (concat "which " const/pip-cmd)) "")
  (error "pip-deps-loader: Can not find %s executable" const/pip-cmd))

(dolist (pkg const/pip-deps)
  (message "Installing package %s" pkg)
  (if (eq 0 (shell-command (concat const/pip-cmd " install " pkg)))
      (message "Installing package %s [DONE]" pkg)
    (error "Installing package %s [FAILED]" pkg)))
