;;; rc/helm.el --- Helm - see http://tuhdo.github.io/helm-intro.html -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- -------------------------------------------------------
;; Key               Definition
;; ----------------- -------------------------------------------------------
;; M-x               Remap standard: Execute command with helm.
;; M-y               Remap standard: Yank with helm.
;; C-x b             Remap standard: Switch buffer with helm.
;; C-x C-f           Remap standard: Find file with helm.
;; C-x C-r           Open recent file with Helm (see also `init-ido.el').
;; C-h b             Describe keybindings using Helm.
;; C-S-r             Search with ripgrep: in current project root.
;;                   See also`init-helm-porojectile.el'
;; C-S-d             Search with Ag: ask for directory first.
;;                   See also`init-helm-porojectile.el'.
;; C-S-s             Helm occur
;; C-x c g           Helm Google suggest.
;; C-c C-p           Edit helm ag/grep/occur etc. search results (after exporting/saving them)
;; C-c h             Open file with helm-projectile (current project).
;; C-c H             Same but first select the project.
;; or C-c M-h
;; C-c e             treemacs: toggle the directory tree.
;; C-c E             Open treemacs with a projectile project.
;; C-S-a             Search with Ag: in current projectile project.
;;                   See also`init-helm.el'.
;; C-S-r             Search with ripgrep: in current projectile project.
;;                   See also`init-helm.el'.

;;; Code:

(use-package helm
  :diminish
  :functions (emaxx--helm-swith-to-buffer-update-sources
              emaxx--helm-switch-to-buffer-completing-read
              emaxx--helm-occur-ensure-input
              emaxx-helm-do-grep-ag-in-directory)
  :init
  (use-package helm-lib
    :ensure helm
    :defer t
    :autoload (helm-this-command
               helm-symbol-name
               helm-get-attr
               helm-set-attr))
  (use-package helm-mode
    :ensure helm
    :defer t
    :autoload (helm-completing-read-default-handler))
  (use-package helm-source
    :ensure helm
    :defer t
    :autoload (helm-make-source))
  (use-package helm-core
    :ensure helm
    :defer t
    :autoload (helm-normalize-sources
               helm-set-local-variable))
  (use-package helm-grep
    :ensure helm
    :defer t
    :autoload (helm-grep-ag))

  (use-package helm-buffers
    :ensure helm
    :defer t
    :autoload (helm-buffer--format-mode-name))

  (defun emaxx--helm-swith-to-buffer-update-sources (&rest args)
    "Copy relevant attributes from a `helm-source-buffers' to `:sources' in ARGS."
    (if-let* ((args (car args))
              ((plistp args))
              (sources (cl-remove-if (lambda (source)
                                       (equal "Unknown candidate"
                                              (helm-get-attr 'name source)))
                                     (helm-normalize-sources
                                      (plist-get args :sources))))
              (source-buffers (helm-make-source "Buffers" 'helm-source-buffers)))
        (progn
          (dolist (source sources)
            (helm-set-attr 'filtered-candidate-transformer
                           (append '(helm-skip-boring-buffers
                                     helm-buffers-sort-transformer)
                                   (helm-get-attr 'filtered-candidate-transformer
                                                  source))
                           source)
            (helm-set-attr 'action
                           (append (let ((action (helm-get-attr 'action source)))
                                     (if (listp action)
                                         (list (car action))
                                       (list action)))
                                   (cdr helm-type-buffer-actions))
                           source)
            (dolist (attr '(keymap
                            persistent-action
                            persistent-help
                            help-message
                            match
                            mode-line
                            heder-line
                            find-file-target))
              (helm-set-attr attr
                             (helm-get-attr attr source-buffers)
                             source)))
          (plist-put args
                     :sources (append sources
                                      (list helm-source-buffer-not-found))))
      args))

  (defun emaxx--helm-switch-to-buffer-completing-read (&rest args)
                                        ; checkdoc-params: (args)
    "Ensure `helm-source-buffers' attributes are used."
    (let* ((current-command (or (helm-this-command) this-command))
           (str-command (if current-command
                            (helm-symbol-name current-command)
                          "completing-read"))
           (buf-name (format "*%s*" str-command)))
      (unwind-protect
          (progn
            (advice-add
             'helm
             :filter-args #'emaxx--helm-swith-to-buffer-update-sources)
            (when-let* ((buffers (all-completions "" (cadr args)))
                        (result (cl-loop for b in buffers
                                         maximize (length b)
                                         into len-buf
                                         maximize (length
                                                   (helm-buffer--format-mode-name b))
                                         into len-mode
                                         finally return (cons len-buf len-mode))))
              (unless (default-value 'helm-buffer-max-length)
                (helm-set-local-variable 'helm-buffer-max-length (car result)))
              (unless (default-value 'helm-buffer-max-len-mode)
                (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result))))

            (apply #'helm-completing-read-default-handler
                   (append args
                           (list str-command buf-name))))
        (advice-remove
         'helm #'emaxx--helm-swith-to-buffer-update-sources))))

  (defun emaxx--helm-occur-ensure-input (args)
    "Add `:input' equal to `:default' to ARGS plist if it's not already there."
    (if (or (not (eq 'helm-occur this-command))
            (not (plistp args))
            (plist-get args :input))
        args
      (plist-put args :input (plist-get args :default))))

  (defun emaxx-helm-do-grep-ag-in-directory (&optional arg)
    "Like `helm-do-grep-ag', but ask for a directory first."
    (interactive "P")
    (let ((dir (helm-read-file-name
                "Search in directory: "
                :test #'file-directory-p
                :default default-directory
                :must-match t)))
      (helm-grep-ag (expand-file-name dir) arg)))

  :custom
  (history-delete-duplicates t)
  (helm-M-x-always-save-history t)
  (helm-M-x-show-short-doc t)
  (completions-detailed t)
  (helm-grep-ag-command emaxx-helm-grep-ag-command)
  ;; Extract value of --color-match argument (if present) when
  ;; `emaxx-helm-grep-ag-command' uses ag.
  (helm-grep-ag-pipe-cmd-switches
   (when (and
          (member
           (when-let* ((cmd (car
                             (cl-remove-if (lambda (str)
                                             (string-match
                                              (rx string-start
                                                  alpha
                                                  (zero-or-more alnum)
                                                  "=")
                                              str))
                                           (split-string
                                            emaxx-helm-grep-ag-command)))))
             (file-name-nondirectory cmd))
           '("ag" "pt"))
          (string-match (rx (group
                             "--color-match"
                             (or "=" (one-or-more space))
                             (+? (not space)))
                            (or "\\" space string-end))
                        emaxx-helm-grep-ag-command))
     (list (match-string 1 emaxx-helm-grep-ag-command))))

  :bind
  (([remap execute-extended-command] . #'helm-M-x) ; M-x
   ([remap yank-pop] . #'helm-show-kill-ring)      ; M-y
   ([remap find-file] . #'helm-find-files)         ; C-x C-f
   ([remap switch-to-buffer] . #'helm-mini)        ; C-x b
   ([remap list-buffers] . #'helm-buffers-list)    ; C-x C-b
   ([remap find-file-read-only] . #'helm-recentf)  ; C-x C-r
   ("C-S-d" . #'emaxx-helm-do-grep-ag-in-directory)
   ("C-S-s" . #'helm-occur)
   ([f10] . helm-buffers-list)
   ([S-f10] . helm-recentf)
   )

  :config
  ;; Do not show these files in helm buffer
  (dolist (pat (list (rx ".tsk" string-end)
                     (rx ".log.")))
    (add-to-list 'helm-boring-file-regexp-list pat))

  (require 'helm-mode)
  (dolist (fun '(switch-to-buffer
                 switch-to-buffer-other-frame
                 switch-to-buffer-other-tab
                 switch-to-buffer-other-window))
    (add-to-list 'helm-completing-read-handlers-alist
                 (cons fun #'emaxx--helm-switch-to-buffer-completing-read)))

  (advice-add #'helm :filter-args #'emaxx--helm-occur-ensure-input)
  (helm-mode))

;; (use-package helm-xref
;;  :requires helm)

(use-package helm-icons
  :after helm
  :requires helm
  :config (helm-icons-enable))

(use-package helm-projectile
  :bind
  (("C-c h"   . #'helm-projectile)
   ("C-c H"   . #'helm-projectile-switch-project)
   ("C-c M-h" . #'helm-projectile-switch-project)
   ("C-S-a"   . #'helm-projectile-ag)
   ("C-S-r"   . #'helm-projectile-rg))

  :config (helm-projectile-on))

(use-package helm-lsp
  :ensure t
  :after (helm lsp)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package helm-flycheck
  :ensure t
  :after (helm flycheck)
  :config
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
;;; helm.el ends here.
