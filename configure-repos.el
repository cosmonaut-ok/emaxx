(defconst const/self-dir (file-name-directory load-file-name))

;;
;; configure "package.el" engine
;;
(message "Configuring package.el...")
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;;
;; configure "el-get" engine
;;

(defconst const/el-get-directory (concat const/self-dir "el-get"))
(defconst const/el-get-user-recipes-directory (concat const/self-dir "el-get/recipes"))
(defconst const/el-get-version "5.1")

;; install el-get itself                                                                                                                                                                           
(when (not (file-directory-p const/el-get-directory))
  (message "Installing el-get...")
  (with-current-buffer
      (url-retrieve-synchronously
       (concat "https://raw.githubusercontent.com/dimitri/el-get/refs/tags/" const/el-get-version "/el-get-install.el"))
    (goto-char (point-max))
    (eval-print-last-sexp))
  (message "Installing el-get [ DONE ]")
  )

;; configure el-get                                                                                                                                                                                
(add-to-list 'load-path (concat const/self-dir "el-get/el-get"))
(require 'el-get)
(add-to-list 'el-get-recipe-path const/el-get-user-recipes-directory)

;;
;; configure 3rd party repos
;;
;; (add-to-list 'load-path (concat const/self-dir "3drparty" t))
