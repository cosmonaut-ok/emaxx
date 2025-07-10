(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "lib: Can not load deps file %s" emacs-user-deps)))

(defun emaxx-browse-url-at-point ()
  "Open an URL at point."
  (interactive)
  (when-let* ((url (thing-at-point 'url)))
    (browse-url url)))

(defun emaxx-package-initialize ()
  (interactive)
  (setq package-archives const/package-archives)
  (package-initialize))

(defun prog-mode-init()
  (interactive)

  (setq-local show-trailing-whitespace t)
  (setq-local electric-pair-open-newline-between-pairs t)
  (setq-local font-lock-maximum-decoration t)
  (setq-local jit-lock-chunk-size 5000)
  (setq-local jit-lock-context-time 0.2)
  (setq-local jit-lock-defer-time .1)
  (setq-local jit-lock-stealth-nice 0.2)
  (setq-local jit-lock-stealth-time 5)
  (setq-local jit-lock-stealth-verbose nil)

  (flycheck-mode t)
  (format-all-mode t)
  (editorconfig-mode t)
  (show-paren-mode t)                   ;show matching parentheses
  ;; Electric pair: automatically close parenthesis, curly brace etc.
  (electric-pair-mode)
  (display-line-numbers-mode t)
  (tab-line-mode t)
  ;; (fci-mode 1) ; FIXME: fill-column-indicator (aka fci-mode) breaks company mode integration
  ;; Font lock mode in prog-mode
  (font-lock-mode t)
  (jit-lock-mode t)
  ;; enable yasnippet minor mode in prog-mode
  (projectile-mode t)
  (yas-minor-mode t)
  )

(defun load-user-rc-file(filename)
  (let* ((expanded-file-name (expand-file-name (locate-user-emacs-file filename)))
         (expanded-file-elc (expand-file-name (locate-user-emacs-file (concat filename ".elc"))))
         (expanded-file-el (expand-file-name (locate-user-emacs-file (concat filename ".el")))))
    (cond ((file-readable-p expanded-file-name) (load expanded-file-name))
          ((file-readable-p expanded-file-elc) (load expanded-file-elc))
          ((file-readable-p expanded-file-el) (load expanded-file-el))
          (t (error "Can not load file %s" filename)))))
;;; lib.el ends here.
