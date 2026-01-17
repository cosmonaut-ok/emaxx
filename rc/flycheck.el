;;; flycheck.el --- Flycheck configuration for Emaxx -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Flycheck see https://www.flycheck.org
;; -------------- -------------------------------------------------------
;; Key            Definition
;; -------------- -------------------------------------------------------
;; C-x c f        `helm-flycheck' show flycheck errors in helm buffer
;; -------------- -------------------------------------------------------

;;; Code:

;; (let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
;;   (if (file-readable-p emacs-user-deps)
;;       (load emacs-user-deps)
;;     (error "flycheck: Can not load deps file %s" emacs-user-deps)))

;; (setq package-archives const/package-archives)
;; (package-initialize)

(require 'cc-mode)
(require 'markdown-mode)

(use-package helm-flycheck
  :commands (emaxx-helm-flycheck)
  :init
  (defun emaxx-helm-flycheck ()
    "Ensure `flycheck-mode' is enabled and run `helm-flycheck'."
    (interactive)
    (unless flycheck-mode
      (flycheck-mode)
      (diminish 'flycheck-mode))
    (helm-flycheck))
  (use-package helm
    :defer t
    :bind
    (:map helm-command-map
          ("f" . #'emaxx-helm-flycheck))))

(use-package poly-rst
  :defer t)

(defvar emaxx--flycheck-mypy-error-codes-alist nil
  "Error codes alist in a form of (MYPY-VERSION . CODES).

The MYPY-VERSION is a symbol of a mypy version and CODES are an
alist in a form of (CODE . BODY).  The CODE is the symbol of a
mypy error code and BODY is a rst body of the code.")

;;; Basic configuration (as need for lsp and for shellcheck)
(use-package flycheck
  :custom
  (flycheck-indication-mode nil)
  (flycheck-highlighting-mode 'lines)
  :functions (emaxx--flycheck-mypy-retrieve-error-codes)
  :autoload (flycheck-add-next-checker
             flycheck-buffer-saved-p
             flycheck-call-checker-process
             flycheck-flake8-fix-error-level
             flycheck-sanitize-errors)
  :custom
  (flycheck-global-modes '(not c++-mode c-mode org-mode))
  :hook
  (after-init . global-flycheck-mode)
  :init
  ;; A custom mypy checker, with error explanations
  (defun emaxx--flycheck-mypy-retrieve-error-codes (mypy-version)
    (let* (error-codes-alist
           (error-headline "^[A-Z][a-z' ]+ \\[\\([a-z-]+\\)\\]
-+"))
      (mapc
       (lambda (error-codes-file-name)
         (with-current-buffer
             (url-retrieve-synchronously
              (concat
               "https://raw.githubusercontent.com/python/mypy/v"
               mypy-version
               "/docs/source/"
               error-codes-file-name))
           (when (re-search-forward "^\\.\\. _error-code\\(?:s\\)?-.*:" nil t)
             (while (re-search-forward error-headline nil t)
               (let* ((error-code (match-string 1))
                      (error-body-start (match-beginning 0))
                      (error-body-end
                       (save-match-data
                         (if (re-search-forward error-headline nil t)
                             (match-beginning 0)
                           (point-max))))
                      (error-body (buffer-substring error-body-start
                                                    error-body-end)))
                 (push (cons (intern error-code) error-body) error-codes-alist)
                 (goto-char error-body-end))))))
       '("error_code_list.rst" "error_code_list2.rst"))
      error-codes-alist))
  :config
  ;; Extended version of https://github.com/flycheck/flycheck/blob/5f2ef17/flycheck.el#L10838-L10860
  (flycheck-define-checker emaxx-python-mypy
    "Mypy syntax and type checker.  Requires mypy>=0.730.

Note that this checker substitutes the original `python-mypy' checker and uses
its configuration variables (i.e., `flycheck-python-mypy-*' variables).

See URL `http://mypy-lang.org/'."
    :command ("mypy"
              "--show-column-numbers"
              "--show-error-codes"
              "--no-pretty"
              (config-file "--config-file" flycheck-python-mypy-config)
              (option "--cache-dir" flycheck-python-mypy-cache-dir)
              (option "--python-executable"
                      flycheck-python-mypy-python-executable)
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line (optional ":" column)
            ": error:" (message)
            (one-or-more (any space)) "[" (id (one-or-more not-newline)) "]"
            line-end)
     (warning line-start (file-name) ":" line (optional ":" column)
              ": warning:" (message)
              (one-or-more (any space)) "[" (id (one-or-more not-newline)) "]"
              line-end)
     (info line-start (file-name) ":" line (optional ":" column)
           ": note:" (message)
           (one-or-more (any space)) "[" (id (one-or-more not-newline)) "]"
           line-end))
    :working-directory flycheck-python-find-project-root
    :modes (python-mode python-ts-mode)
    ;; Ensure the file is saved, to work around
    ;; https://github.com/python/mypy/issues/4746.
    :predicate flycheck-buffer-saved-p
    :error-explainer
    (lambda (error)
      (when-let* ((error-code (flycheck-error-id error))
                  (mypy-version
                   (replace-regexp-in-string
                    "mypy \\(\\(?:[0-9]\\.\\)+[0-9]\\).*\n"
                    "\\1"
                    (shell-command-to-string "mypy --version")))
                  (error-codes-alist
                   (emaxx-setf-when-nil
                    (alist-get (intern mypy-version)
                               emaxx--flycheck-mypy-error-codes-alist)
                    (emaxx--flycheck-mypy-retrieve-error-codes
                     mypy-version)))
                  (explanation (alist-get (intern error-code)
                                          error-codes-alist)))
        (lambda ()
          (with-current-buffer standard-output
            (insert explanation)
            (poly-rst-mode)
            (view-mode)
            (font-lock-flush)
            (font-lock-ensure))))))

  (add-to-list 'flycheck-checkers 'emaxx-python-mypy)
  (mapc (lambda (checker)
          (flycheck-add-next-checker checker '(warning . emaxx-python-mypy)))
        '(python-flake8 python-pylint python-pycompile))
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list 'flycheck-disabled-checkers 'python-mypy)))

  ;; A custom ruff checker with error explanations
  ;; Extended version of https://gist.github.com/abo-abo/277d1fe1e86f0e46d3161345f26e8f3a
  (flycheck-define-checker emaxx-python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-emaxx-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff" "check"
              (eval (let ((ruff-version
                           (replace-regexp-in-string
                            (rx "ruff "
                                (group (one-or-more digit)
                                       (one-or-more (seq "." (one-or-more digit)))))
                            (rx (backref 1))
                            (string-trim (shell-command-to-string "ruff --version")))))
                      (cond
                       ((version< ruff-version "0.1")
                        '("--format" "text"))
                       ((version< ruff-version "0.5")
                        '("--output-format" "text"))
                       (t
                        '("--output-format" "concise")))))
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter
    (lambda (errors)
      (let ((errors (flycheck-sanitize-errors errors)))
        (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((error line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha digit))) ": "
            (message (one-or-more not-newline)))
     (warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :error-explainer
    (lambda (error)
      (when-let* ((error-code (flycheck-error-id error))
                  (error-level (flycheck-error-level error)))
        (if (eq error-level 'error)
            (message "No explanation for error: %s" error-code)
          (lambda ()
            (flycheck-call-checker-process
             'emaxx-python-ruff nil standard-output t "rule" error-code)
            (with-current-buffer standard-output
              (let ((markdown-fontify-code-block-default-mode 'python-mode)
                    (markdown-fontify-code-blocks-natively t)
                    (markdown-hide-markup t))
                (ignore markdown-fontify-code-block-default-mode
                        markdown-fontify-code-blocks-natively
                        markdown-hide-markup)
                (markdown-view-mode)
                (font-lock-flush)
                (font-lock-ensure)))))))
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'emaxx-python-ruff)
  (flycheck-add-next-checker 'emaxx-python-ruff '(warning . emaxx-python-mypy))
  ;; (add-to-list 'flycheck-shellcheck-supported-shells 'ksh93)
  )

(use-package flycheck-pos-tip
  :after flycheck
  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
  :custom (flycheck-pos-tip-timeout 30))

(use-package flycheck-relint
  :after (flycheck)
  :config
  (flycheck-relint-setup))

(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))

;;; flycheck.el ends here
