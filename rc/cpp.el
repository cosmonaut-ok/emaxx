(use-package cc-mode
  :init
  (defun call-interactivaly-compile ()
    (interactive)
    (setq-local compilation-read-command nil)
    (call-interactively 'compile))
  (defun c-modes-init ()
    (lsp)
    (lsp-ui-mode t)
    (dap-mode t)
    (yas-minor-mode t)
    (projectile-mode t)
    (format-all-mode t)

    ;;
    ;; customizations
    ;;
    ;; NOTE:Available C style:
    ;; “gnu”: The default style for GNU projects
    ;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
    ;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
    ;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
    ;; “stroustrup”: What Stroustrup, the author of C++ used in his book
    ;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,”
    ;;             Erik Nyquist and Mats Henricson, Ellemtel
    ;; “linux”: What the Linux developers use for kernel development
    ;; “python”: What Python developers use for extension modules
    ;; “java”: The default style for java-mode (see below)
    ;; “user”: When you want to define your own style
    (c-set-style "bsd")    
    )
  :hook ((c-mode c++-mode) . c-modes-init)
  :config
  ;; (define-key c-mode-map  [(control tab)] 'company-complete)
  ;; (define-key c++-mode-map  [(control tab)] 'company-complete)
  :custom
  (c-default-style "bsd")
  (c-basic-offset 2)
  (tab-width 2)
  (indent-tabs-mode nil)
  :bind (("<f5>" . call-interactivaly-compile))
)

;; ;; dap-mode -- emacs client/library for Debug Adapter Protocol is a wire protocol
;; ;; for communication between client and Debug Server. It’s similar to the LSP
;; ;; but provides integration with debug server.
;; (use-package dap-mode
;;   :init (defun dap-init()
;; 	  (call-interactively #'dap-hydra))
;;   (setq dap-auto-configure-features '(sessions locals controls tooltip))
;;   :hook (dap-stopped . dap-init)
;;   )
;;   ;; (add-hook 'dap-stopped-hook
;;   ;;         (lambda (arg) (call-interactively #'dap-hydra)))

;; ;; cwarn -- is an Emacs minor mode that highlights potentially problematic C and C++ code constructs
;; (use-package cwarn
;;   :ensure t
;;   :hook ((c-mode c++-mode) . cwarn-mode))

;; ;; modern (C++20) C++ font lock
;; (use-package modern-cpp-font-lock
;;   :ensure t
;;   :config
;;   (modern-c++-font-lock-global-mode t))

;; (use-package company-c-headers
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-c-headers))

;; ;; ;; (use-package flycheck
;; ;; ;;   :ensure t)
;; ;; ;; (use-package yasnippet
;; ;; ;;   :ensure t
;; ;; ;;   :config (yas-global-mode))
;; ;; ;; (use-package which-key
;; ;; ;;   :ensure t
;; ;; ;;   :config (which-key-mode))
;; ;; ;; (use-package helm-lsp
;; ;; ;;   :ensure t)
;; ;; ;; (use-package helm
;; ;; ;;   :ensure t
;; ;; ;;   :config (helm-mode))
;; ;; (use-package lsp-treemacs
;; ;;   :ensure t)

;; ;; ;;; This will enable emacs to compile a simple cpp single file without any makefile by just pressing [f9] key
;; ;; (defun code-compile()
;; ;;   (interactive)
;; ;;   (unless (file-exists-p "Makefile")
;; ;;     (set (make-local-variable 'compile-command)
;; ;; 	 (let ((file (file-name-nondirectory buffer-file-name)))
;; ;; 	   (format "%s -o %s %s"
;; ;; 		   (if (equal (file-name-extension file) "cpp") "g++" "gcc")
;; ;; 		   (file-name-sans-extension file)
;; ;; 		   file)))
;; ;;     (compile compile-command)))
;; ;; (global-set-key [f9] 'code-compile)
