;; This file is for loading nodejs npm packages,
;; defined in `deps.el` declarative file

(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "npm-deps-loader: Can not load deps file %s" emacs-user-deps)))

(defconst const/npm-cmd "npm")

(when (equal (shell-command-to-string (concat "which " const/npm-cmd)) "")
  (error "node-deps-loader: Can not find %s executable" const/npm-cmd))

(dolist (pkg const/npm-deps)
  (message "Installing package %s" pkg)
  (if (eq 0 (shell-command (concat const/npm-cmd " -g install " pkg)))
      (message "Installing package %s [DONE]" pkg)
    (error "Installing package %s [FAILED]" pkg)))
