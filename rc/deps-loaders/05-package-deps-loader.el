;; This file is for loading emacs `package` packages,
;; defined in `deps.el` declarative file

(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "package-deps-loader: Can not load deps file %s" emacs-user-deps)))

;; initialize package with preconfigured package-archives
;; defined in deps.el
(setq package-archives const/package-archives)
(package-initialize)

;;
;; install package.el`s packages
;;
(message "Installing package.el packages...")
(package-refresh-contents)
(dolist (i const/package-packages-list)
  (unless (package-installed-p i)
    (package-install i)))
(message "Installing package.el packages [ DONE ]")

;; download emoji
(message "Downloading emoji")
(require 'emojify)
(emojify-set-emoji-data)
(emojify-download-emoji emojify-emoji-set)
(message "Downloading emoji [DONE]")

;; download all-the-icons
(message "Downloading additional icons")
(require 'all-the-icons)
(all-the-icons-install-fonts t)
(message "Downloading additional icons [DONE]")
