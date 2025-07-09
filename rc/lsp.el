(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c C-l")
  :hook (lsp-mode-hook . lsp-enable-which-key-integration)
  ;; :bind (:map lsp-mode-map ("C-c l" . lsp-command-map))
  :config
  ;; (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  ;; add some manual LSP server bindings
  (add-to-list 'lsp-language-id-configuration '(autoconf-mode . "autotools"))
  (add-to-list 'lsp-language-id-configuration '(makefile-automake-mode . "autotools"))
  
  (let (
	;; remember name of current buffer
	;; to switch to it later
	(curr-buf (current-buffer)))

    (lsp-treemacs-errors-list)
    (lsp-treemacs-symbols)

    ;; switch back to current buffer
    ;; after additional windows activated
    (switch-to-buffer curr-buf))

  ;; (treemacs-select-window)

  ;; exclude lsp-treemacs error list and symbols
  ;; from other-window command
  (dolist (w (window-list))
    (let ((buf-name (buffer-name (window-buffer w))))
      (when (or
	     (equal "*LSP Error List*" buf-name)
	     (equal "*LSP Symbols List*" buf-name))
	(set-window-parameter w 'no-other-window t))))

  :commands lsp)

;;   ;; :custom
;;   (custom-set-variables
;;    '(lsp-ui-sideline-show-code-actions t)
;;    '(lsp-ui-sideline-delay 2)
;;    '(lsp-ui-peek-enable t)
;;    '(lsp-ui-doc-enable t)
;;    '(lsp-ui-doc-delay 2)
;;    '(lsp-ui-doc-show-with-mouse t)
;;    '(lsp-ui-imenu-window-width 180)
;;    '(lsp-ui-imenu-auto-refresh t)
;;    )

;; (use-package lsp-ui
;;   :ensure t
;;   :bind
;;   (:map lsp-ui-mode-map
;;         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;         :map lsp-ui-mode-map
;;         ([remap xref-find-references] . lsp-ui-peek-find-references))
;;   :config
;;   ;; (lsp-ui-mode)
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   :commands lsp-ui-mode
;;   :after lsp)

;; ;; (with-eval-after-load 'lsp-ui-mode
;; ;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; ;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
