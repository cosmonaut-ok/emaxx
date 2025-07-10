;; This file is for loading python`s go packages,
;; defined in `deps.el` declarative file

(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "go-deps-loader: Can not load deps file %s" emacs-user-deps)))

(defconst const/go-cmd "go")

(when (equal (shell-command-to-string (concat "which " const/go-cmd)) "")
  (error "go-deps-loader: Can not find %s executable" const/go-cmd))

(dolist (pkg const/go-deps)
  (message "Installing package %s" pkg)
  (if (eq 0 (shell-command (concat const/go-cmd " install " pkg)))
      (message "Installing package %s [DONE]" pkg)
    (error "Installing package %s [FAILED]" pkg)))
