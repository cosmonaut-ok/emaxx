(defconst const/self-dir (file-name-directory load-file-name))

(load (concat const/self-dir "configure-repos.el"))

;;
;; declare package.el`s package list and el-get`s package list
;;
(defconst const/package-packages-list
  '(
    ;; beautify a bit
    all-the-icons
    emojify			  ;add a bit of emoji
    dashboard			  ;replace for default startup screen
    dashboard-hackernews	  ;add news to dashboard
    material-theme		  ;use it as default theme
    exec-path-from-shell	  ;ensure environment variables inside Emacs look the same as in the user's shell.
    saveplace			  ;When you visit a file, point goes to the last place where it was when you previously visited the same file
    saveplace-pdf-view
    bookmark
    pdf-tools
    markdown-mode
    company
    company-ansible
    company-arduino
    company-auctex
    company-bibtex
    company-c-headers
    company-math
    company-quickhelp
    company-quickhelp-terminal
    company-reftex
    company-terraform
    company-web
    ;; yasnippet
    yasnippet
    yasnippet-snippets
    ;; treemacs-related packages
    treemacs
    treemacs-all-the-icons
    treemacs-magit
    ;; lsp-mode, related and special lsp modes
    lsp-mode
    lsp-ui
    lsp-treemacs
    lsp-latex
    lsp-jedi				;python-jedi language server
    lsp-docker
    modern-cpp-font-lock
    clang-format			;c/c++ language server
    dap-mode				;Emacs client/library for Debug Adapter Protocol is a wire protocol
					; for communication between client and Debug Server. It’s similar to
					; the LSP but provides integration with debug server.
    flycheck				;flycheck mode
    ;;
    google-translate
    ;;
    yasnippet
    format-all
    projectile
    helm
    helm-projectile
    ))

(defconst const/el-get-packages-list
  '(
    "company-lsp"
    ))

(defconst const/third-party-packages-list
  '(
    ("powershell-mode.el" "https://www.emacswiki.org/emacs/download/PowerShell-Mode.el")
    ("go-template-mode.el" "https://gist.githubusercontent.com/grafov/10985431/raw/dcd12037308446179f26f7d2ab2c034a1e995d2e/go-template-mode.el")
    ("kubernetes-helm.el" "https://raw.githubusercontent.com/abrochard/kubernetes-helm/master/kubernetes-helm.el")
    ))

;;
;; install package.el`s packages
;;
(message "Installing package.el packages...")
(package-refresh-contents)
(dolist (i const/package-packages-list)
  (unless (package-installed-p i)
    (package-install i)))
(message "Installing package.el packages [ DONE ]")

;;
;; install el-get`s packages
;;
(message "Installing el-get packages...")
(el-get 'sync const/el-get-packages-list) ;; install packages
(message "Installing el-get packages [ DONE ]")

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

(defconst const/third-party-dir (concat const/self-dir "3drparty"))

;; create directory for third party scripts
(make-directory const/third-party-dir t)
(dolist (c const/third-party-packages-list)
  (when (not (file-exists-p (concat const/third-party-dir "/" (car c))))
    (download-file (cadr c) (concat const/third-party-dir "/" (car c)))))

;; download emoji also
(require 'emojify)
(emojify-set-emoji-data)
(emojify-download-emoji emojify-emoji-set)

(require 'all-the-icons)
(all-the-icons-install-fonts t)
