;; This file is for loading python`s gem packages,
;; defined in `deps.el` declarative file

(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "gem-deps-loader: Can not load deps file %s" emacs-user-deps)))

(defconst const/gem-cmd "gem")

(when (equal (shell-command-to-string (concat "which " const/gem-cmd)) "")
  (error "gem-deps-loader: Can not find %s executable" const/gem-cmd))

(dolist (pkg const/gem-deps)
  (message "Installing package %s" pkg)
  (if (eq 0 (shell-command (concat const/gem-cmd " install " pkg)))
      (message "Installing package %s [DONE]" pkg)
    (error "Installing package %s [FAILED]" pkg)))
