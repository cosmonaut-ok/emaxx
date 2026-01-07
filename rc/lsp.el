;;; lsp.el --- lsp configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c C-l")
  (setq-default lsp-clients-clangd-executable
                (seq-find #'executable-find emaxx-lsp-clangd-executable)) ; TODO: new record
  ;; :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
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

    ;; exclude lsp-treemacs error list and symbols
    ;; from other-window command
    (dolist (w (window-list))
      (let ((buf-name (buffer-name (window-buffer w))))
        (when (or
               (equal "*LSP Error List*" buf-name)
               (equal "*LSP Symbols List*" buf-name))
          (set-window-parameter w 'no-delete-other-windows t)
          (set-window-parameter w 'no-other-window t))))

    ;; switch back to current buffer
    ;; after additional windows activated
    (switch-to-buffer-other-window curr-buf))
  ;; (treemacs-select-window)

  :custom
  ;; --------------------------------------------------------------------------------
  (lsp-diagnostic-provider :flycheck)
  (lsp-flycheck-live-reporting t)
  ;; company mode configuration for lsp-mode
  (lsp-completion-provider :capf)
  ;; semantic hilite via lsp server
  (lsp-semantic-tokens-enable t)

  (lsp-idle-delay 0.2) ;; clangd is fast
  (lsp-diagnostics-delay 0.5) ; Delay diagnostics by 0.5 seconds
  (lsp-completion-delay 0.1)  ; Delay completion requests
  (lsp-log-io nil)
  ;; --------------------------------------------------------------------------------

  :commands lsp)

(use-package lsp-ui
  :ensure t
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        :map lsp-ui-mode-map
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  ;; (lsp-ui-mode)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-delay 2)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover nil)

  (lsp-ui-imenu-window-width 180)
  (lsp-ui-imenu-auto-refresh t)

  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 2)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-include-signature t)

  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'bottom)

  (lsp-ui-peek-enable t)

  (lsp-lens-enable t)

  :after lsp)

(with-eval-after-load 'lsp-ui-mode
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; --------------------------------------------------------------------------------

(use-package helm-lsp
  :after (lsp-mode helm)
  :commands
  (helm-lsp-workspace-symbol
   helm-lsp-global-workspace-symbol
   helm-lsp-code-actions))
;;; lsp.el ends here.
