;; This file is for loading emacs `el-get` packages,
;; defined in `deps.el` declarative file

(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "el-get-deps-loader: Can not load deps file %s" emacs-user-deps)))

(defconst const/el-get-directory (expand-file-name (locate-user-emacs-file "el-get")))
(defconst const/el-get-user-recipes-directory (expand-file-name (locate-user-emacs-file (file-name-concat "el-get" "recipes"))))
(defconst const/el-get-version "master")   ;refs/tags/5.1

;; install el-get itself
(when (not (file-directory-p const/el-get-directory))
  (message "Installing el-get...")
  (with-current-buffer
      (url-retrieve-synchronously
       (concat "https://raw.githubusercontent.com/dimitri/el-get/" const/el-get-version "/el-get-install.el"))
    (goto-char (point-max))
    (eval-print-last-sexp))
  (message "Installing el-get [ DONE ]")
  )

;; configure el-get
(add-to-list 'load-path (file-name-concat const/el-get-directory "el-get"))
(require 'el-get)
(add-to-list 'el-get-recipe-path const/el-get-user-recipes-directory)

;;
;; install el-get`s packages
;;
(message "Installing el-get packages...")
(el-get 'sync const/el-get-packages-list) ;; install packages
(message "Installing el-get packages [ DONE ]")
