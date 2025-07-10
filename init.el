;;; init.el --- this is the initial file for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB). This reduces
;; the startup time.
(setq gc-cons-threshold 100000000)

;; limit lower emacs version by 30.X
;; I don't want to support legacy versions
;; at least for now
(let ((min-version "30"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs-%s, but you're running %s"
           min-version emacs-version)))

;;;
;;; global constants
;;;
;; load custom code before main initialization
(defconst const/before-init-file "before-init.el"
  "Name of the before init file.")

;; load custom code after main initialization
(defconst const/after-init-file "after-init.el"
  "Name of the after init file.")

(defconst const/custom-file "emaxx-custom.el"
  "Name of the customization file.")

(defconst const/rc-dir
  (expand-file-name (locate-user-emacs-file "rc"))
  "Location of the modules directory.")

(defconst const/third-party-dir
  (expand-file-name (locate-user-emacs-file "3rd-party"))
  "Location of the third party directory.")

;;
;; helper functions
;;
(require 'bytecomp)
(defun emaxx-recompile-rc ()
  "Recompile modules.
For which the .elc is older than the .el, if the .elc exists.
Also discard .elc without corresponding .el."
  (interactive)
  (dolist (dir (list const/rc-dir
                     const/third-party-dir))
    (when (file-directory-p dir)
      ;; (Re)compile
      (dolist (el (directory-files dir t "\\.el\\'"))
        (let ((elc (byte-compile-dest-file el)))
          (when (or (not (file-exists-p elc))
                    (file-newer-than-file-p el elc))
            (if (not (file-exists-p elc))
                (message "Compile %s" el)
              (message "Recompile %s" el))
            (byte-compile-file el))))
      ;; Discard .elc singletons
      (dolist (elc (directory-files dir t "\\.elc\\'"))
        (let ((el (concat (concat (file-name-sans-extension elc) ".el"))))
          (unless (file-exists-p el)
            (warn "Removing singleton .elc file: %s" elc)
            (delete-file elc)))))))

;;
;; very early init
;;
;; Save any custom set variable in const/custom-file rather than at the end of init.el
(setq custom-file const/custom-file)

;; add directory with 3rd party packages to load-path
(add-to-list 'load-path const/third-party-dir)

;; load generic library file initially
(let ((emacs-user-lib (expand-file-name (locate-user-emacs-file (file-name-concat "rc" "lib.el")))))
  (if (file-readable-p emacs-user-lib)
      (load emacs-user-lib)
    (error "init: Can not load library file %s" emacs-user-lib)))

;; load customizations (defcustoms and deffaces defined in rc/custom)
(load-user-rc-file (file-name-concat "rc" "custom"))

;; load custom code (if any)
;; before main init proces
;; NOTE: it can use predefined customs, faces and library functions
(let ((before-init-file-name (expand-file-name (locate-user-emacs-file const/before-init-file))))
  (when (file-readable-p before-init-file-name)
    (message "Loading before-init file: %s" before-init-file-name)
    (load before-init-file-name)))

;; load saved customizations file
(when (file-readable-p (locate-user-emacs-file const/custom-file))
  (load-user-rc-file const/custom-file))

;;
;; main initialization
;;
(emaxx-recompile-rc)

;; (load-user-rc-file (file-name-concat "rc" "lib")) ;some library functions for internal usage

(load-user-rc-file (file-name-concat "rc" "look-and-feel"))
(load-user-rc-file (file-name-concat "rc" "powerline")) ;look and feel also :-P
(load-user-rc-file (file-name-concat "rc" "help"))
(load-user-rc-file (file-name-concat "rc" "company"))
(load-user-rc-file (file-name-concat "rc" "helm"))
(load-user-rc-file (file-name-concat "rc" "projectile"))
(load-user-rc-file (file-name-concat "rc" "lsp"))

;; declarative modes
(load-user-rc-file (file-name-concat "rc" "markdown"))
;; (load-user-rc-file (file-name-concat "rc" "org"))
(load-user-rc-file (file-name-concat "rc" "json"))
(load-user-rc-file (file-name-concat "rc" "yaml"))
(load-user-rc-file (file-name-concat "rc" "xml"))
(load-user-rc-file (file-name-concat "rc" "ansible"))

;; programming language modes
(load-user-rc-file (file-name-concat "rc" "shell"))
(load-user-rc-file (file-name-concat "rc" "cpp"))
(load-user-rc-file (file-name-concat "rc" "csharp"))
(load-user-rc-file (file-name-concat "rc" "js"))
(load-user-rc-file (file-name-concat "rc" "python"))
;; (load-user-rc-file (file-name-concat "rc" "ruby"))
(load-user-rc-file (file-name-concat "rc" "elisp"))
(load-user-rc-file (file-name-concat "rc" "groovy"))
(load-user-rc-file (file-name-concat "rc" "powershell"))

;; tooling modes
(load-user-rc-file (file-name-concat "rc" "yasnippet"))
(load-user-rc-file (file-name-concat "rc" "autotools"))
(load-user-rc-file (file-name-concat "rc" "docker"))
(load-user-rc-file (file-name-concat "rc" "k8s"))
(load-user-rc-file (file-name-concat "rc" "terraform"))
(load-user-rc-file (file-name-concat "rc" "flycheck"))
(load-user-rc-file (file-name-concat "rc" "system"))
(load-user-rc-file (file-name-concat "rc" "util"))
(load-user-rc-file (file-name-concat "rc" "treemacs"))

;; load custom code (if any)
;; after main init proces
(let ((after-init-file-name (expand-file-name (locate-user-emacs-file const/after-init-file))))
  (when (file-readable-p after-init-file-name)
    (message "Loading after-init file: %s" after-init-file-name)
    (load after-init-file-name)))

(defun treemacs-start ()
  "Another nonce menu function."
  (interactive)
  (dashboard-open)                      ;do it again, because hackernews are not starting first time
  (treemacs))

(tool-bar-add-item "spell" 'treemacs-start
		               'treemacs-start
		               :help   "Launch treemacs")
;;; init.el ends here.
