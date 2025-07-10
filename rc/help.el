;;; help.el --- Help extensions                 -*- lexical-binding: t -*-
;;; adopted from exordium emacs config set

;;; Commentary:
;;
;; ----------------- ---------------------------------------------------------
;; Key               Definition
;; ----------------- ---------------------------------------------------------
;; C-c C-o           Open URL at point (in `help-mode' and `helpful-mode')
;; C-h f             Show help for function, macro or special form
;; C-h F             Show help for function
;; C-h v             Show help for variable
;; C-h k             Show help for interactive command bound to key sequence
;; C-h C             Show help for interactive command
;; C-j               Show help for currentlyselected candidate, when completing
;;                   read for `helpful' commands
;; C-c C-d           Show help for thing at point (in `emacs-lisp-mode')
;; C-o               Show a `casual' transient, which one depends on mode.
;; C-c =             Run `difftastic-dired-diff' in `dired-mode'.

;;; Code:

;;; load generic library initially
(let ((emacs-user-lib (expand-file-name (locate-user-emacs-file (file-name-concat "rc" "lib.el")))))
  (if (file-readable-p emacs-user-lib)
      (load emacs-user-lib)
    (error "look-and-feel: Can not load library file %s" emacs-user-lib)))

;; show the key bindings following currently entered incomplete command
(use-package which-key :config (which-key-mode))

;; Tune keys in `help-mode' - i.e., works when reading package information in
;; `package-list-packages'.

(use-package help-mode
  :ensure nil
  :bind
  (:map help-mode-map
   ("C-c C-o" . #'emaxx-browse-url-at-point)))

(use-package helpful
  :functions (emaxx--helpful-persistent-action
              emaxx--helm-helpful-completing-read)
  :init
  (use-package helm
    :defer t
    :custom
    (helm-describe-variable-function #'helpful-variable)
    (helm-describe-function-function #'helpful-function))
  (use-package helm-mode
    :ensure helm
    :defer t
    :autoload (helm-completing-read-default-handler))

  (defun emaxx--helpful-pop-to-buffer (buffer)
    "Pop to BUFFER in the same window if it is a Helpful window.
Otherwise pop to buffer (presumably in a new window)."
    (if (derived-mode-p 'helpful-mode)
        (pop-to-buffer-same-window buffer)
      (pop-to-buffer buffer)))

  (defun emaxx--helpful-persistent-action (type)
    "Generate a function that adds `:persistent-action' TYPE to args."
    (lambda (&rest args)
      (plist-put (plist-put (car args)
                            :persistent-help
                            (format "Describe %s" type))
                 :persistent-action
                 (lambda (candidate)
                   (pcase (intern-soft candidate)
                     ((and (pred fboundp) (pred boundp) sym)
                      (if (eq type 'variable)
                          (helm-describe-variable sym)
                        (helm-describe-function sym)))
                     ((and (pred boundp) sym)
                      (helm-describe-variable sym))
                     ((and (pred fboundp) sym)
                      (helm-describe-function sym))
                     ((and (pred facep) sym)
                      (helm-describe-face sym)))))))

  (defun emaxx--helm-helpful-completing-read (&rest args)
                                        ; checkdoc-params: (args)
    "Ensure affixation and persistent actions are used."
    (let* ((current-command (or (helm-this-command) this-command))
           (str-command (if current-command
                            (helm-symbol-name current-command)
                          "completing-read"))
           (buf-name (format "*%s*" str-command))
           (type (cond
                  ((eq current-command 'helpful-variable) 'variable)
                  ((eq current-command 'helpful-symbol) 'symbol)
                  (t 'function)))
           (persistent-action (emaxx--helpful-persistent-action type)))
      (unwind-protect
          (let ((completion-extra-properties
                 '(:affixation-function
                   helm-symbol-completion-table-affixation)))
            (advice-add 'helm-comp-read
                        :filter-args persistent-action)
            (apply #'helm-completing-read-default-handler
                   (append args
                           (list str-command buf-name))))
        (advice-remove 'helm-comp-read persistent-action))))

  :custom
  ;; By default `show-paren-mode' is disabled in modes deriving from
  ;; `special-mode'.  Enable it for `helpful' if it doesn't match
  ;; the `show-paren-predicate'
  (show-paren-predicate (if (with-temp-buffer
                              (require 'helpful nil t)
                              (helpful-mode)
                              (buffer-match-p show-paren-predicate
                                              (current-buffer)))
                            show-paren-predicate
                          (list 'or '(derived-mode . helpful-mode)
                                show-paren-predicate)))
  (helpful-switch-buffer-function #'emaxx--helpful-pop-to-buffer)
  (completions-detailed t)

  :bind
  (;; Note that the built-in `describe-function' includes both functions
   ;; and macros. `helpful-function' is functions only, so we provide
   ;; `helpful-callable' as a drop-in replacement.
   ("C-h f" . #'helpful-callable)
   ;; Look up *F*unctions (excludes macros).
   ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
   ;; already links to the manual, if a function is referenced there.
   ("C-h F" . #'helpful-function)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ;; Look up *C*ommands.
   ;; By default, C-h C is bound to describe `describe-coding-system'.
   ;; Apparently it's frequently useful to only look at interactive functions.
   ("C-h C" . #'helpful-command)
   :map helpful-mode-map
   ("C-c C-d" . #'helpful-at-point)
   ("C-c C-o" . #'emaxx-browse-url-at-point))
  :config
  (require 'helm-mode)
  (dolist (fun '(helpful-callable
                 helpful-command
                 helpful-function
                 helpful-macro
                 helpful-variable
                 helpful-symbol))
    (add-to-list 'helm-completing-read-handlers-alist
                 (cons fun #'emaxx--helm-helpful-completing-read))))
;;; help.el ends here
