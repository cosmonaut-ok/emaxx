(defconst const/self-dir (file-name-directory load-file-name))

(defconst const/load-components
  '(
    "generic" 				; general purpose config
    "lsp"				; language server protocol general configuration
    "treemacs"				; treemacs-related configuration (left menu window)
    ;; "company"				; company mode (autocomplete windows)
    ;; "yasnippet"				; yasnippet snippets support
    ;; "markdown"
    ;; "google-translate"			; google-translate package
    ;; "autotools"				; make/makefile/autoconf/automake-related configuration
    "cpp"				; C++-related configuration
    ))

(dolist (c const/load-components)
  (load (concat const/self-dir c)))

;; (server-start)
