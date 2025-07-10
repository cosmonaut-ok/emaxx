;;; markdown.el ---  Markdown http://jblevins.org/projects/markdown-mode/ -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :ensure t
  :mode (
         ("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         )
  :custom
  ;; always open the preview window at the right
  (markdown-split-window-direction 'right)
  ;; delete exported HTML file after markdown-live-preview-export is called
  (markdown-live-preview-delete-export 'delete-on-export)
  (markdown-enable-math t)
  (markdown-command "multimarkdown")
  (markdown-enable-html t)
  :hook (markdown-mode . font-lock-mode)
  ;; :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))
;;; markdown.el ends here
