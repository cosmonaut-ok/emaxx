;;; look-and-feel.el --- general configuration of user interface for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;

;;; load generic library initially
(let ((emacs-user-lib (expand-file-name (locate-user-emacs-file (file-name-concat "rc" "lib.el")))))
  (if (file-readable-p emacs-user-lib)
      (load emacs-user-lib)
    (error "look-and-feel: Can not load library file %s" emacs-user-lib)))

;;; Ensure environment variables inside Emacs
;;; look the same as in the user's shell
;;;
(use-package exec-path-from-shell
  :init
  (setenv "SHELL" "/bin/bash")
  :ensure t
  :if (memq window-system '(mac ns x))
  ;; :custom
  ;; (exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))
  :config
  (dolist (var '("PATH" "GOPATH" "PYTHONPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;;
;;; Fonts
;;;
(defun emaxx-available-preferred-fonts ()
  "Trim the unavailable fonts from the preferred font list."
  (cl-remove-if-not (lambda (font-and-size)
                      (member (car font-and-size) (font-family-list)))
                    emaxx-preferred-fonts))

(defun emaxx-font-size ()
  "Find the available preferred font size."
  (when (emaxx-available-preferred-fonts)
    (cdar (emaxx-available-preferred-fonts))))

(defun emaxx-font-name ()
  "Find the avaliable preferred font name."
  (when (emaxx-available-preferred-fonts)
    (caar (emaxx-available-preferred-fonts))))

(defun emaxx-set-font (&optional font size)
  "Find the preferred fonts that are available and choose the first one.
Set FONT and SIZE if they are passed as arguments."
  (interactive
   (list (completing-read (format "Font (default %s): " (emaxx-font-name))
                          (emaxx-available-preferred-fonts) nil nil nil nil
                          (emaxx-font-name))
         (read-number "Size: " (emaxx-font-size))))
  (let ((font (or font (emaxx-font-name)))
        (size (or size (emaxx-font-size))))
    (when (and font size)
      (message "Setting font family: %s, height: %s" font size)
      (set-face-attribute 'default nil
                          :family font
                          :height size
                          :weight 'normal)
      t))) ;; indicate that the font has been set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set preferred encoding
(prefer-coding-system 'utf-8)

(when emaxx-preferred-fonts
  (emaxx-set-font))

(when (daemonp)
  (add-hook 'server-after-make-frame-hook #'emaxx-set-font))

;;;
;;; User Interface
;;;

;; launch in fullscreen mode
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; ;;; Remove the tool bar
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))

;; ;;; Remove the menu bar
;; (when (fboundp 'menu-bar-mode)
;;   (menu-bar-mode -1))

;; ;;; Remove the scrollbar completely
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))

;;; Only show the menu bar in a graphical window
;;; (we don't want to loose that top line in a tty)
(menu-bar-mode (if (null (window-system)) -1 1))

;;; Disable blinking cursor
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;;; Display column number in the modebar
(column-number-mode 1)

;;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t)

;;; Better frame title with buffer name
(setq frame-title-format (concat "%b - emaxx@" (system-name)))

;;; Disable beep
(setq visual-bell t)

;;; Colorize selection
(transient-mark-mode 'on)

;;; Autofill at 79 characters
(setq-default fill-column emaxx-fill-column-number)


;; Show only 1 window on startup (useful if you open multiple files)
(add-hook 'emacs-startup-hook #'delete-other-windows t)

;;; Wordwrap at word boundadies
(global-visual-line-mode 1)

;; Remove surrounding quotes for link buttons and stick to the same window
(use-package help
  :ensure nil
  :defer t
  :custom
  (help-clean-buttons t)
  (help-window-keep-selected t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: learn WTF (from exordium)
;; (use-package paren
;;   :ensure nil
;;   :if (version<= "29" emacs-version)
;;   :functions (emaxx--show-paren-tune-child-frame-context)
;;   :init
;;   (defun emaxx--show-paren-tune-child-frame-context (&rest _)
;;     "Set face `child-frame-border' and add matching paren overlay."
;;     (when-let* ((frame show-paren--context-child-frame)
;;                 (overlay-face (overlay-get show-paren--overlay-1 'face)))
;;       (set-face-attribute 'child-frame-border frame
;;                           :background
;;                           (face-attribute overlay-face :background nil t))
;;       (when-let* (((eq show-paren-style 'parenthesis))
;;                   (data (funcall show-paren-data-function))
;;                   (pos (min (nth 0 data) (nth 2 data)))
;;                   (pos (save-excursion
;;                          ;; The following is adjusted from
;;                          ;; `blink-paren-open-paren-line-string'
;;                          (goto-char pos)
;;                          (cond
;;                           ;; Context is what precedes the open in its line, if
;;                           ;; anything.
;;                           ((save-excursion (skip-chars-backward " \t")
;;                                            (not (bolp)))
;;                            (- (1+ pos) (line-beginning-position)))
;;                           ;; Context is what follows the open in its line, if
;;                           ;; anything
;;                           ((save-excursion (forward-char 1)
;;                                            (skip-chars-forward " \t")
;;                                            (not (eolp)))
;;                            1)
;;                           ;; Context is the previous nonblank line, if there is
;;                           ;; one.
;;                           ((save-excursion (skip-chars-backward "\n \t")
;;                                            (not (bobp)))
;;                            (+  (- (- (progn (skip-chars-backward "\n \t")
;;                                             (line-beginning-position))
;;                                      (progn (end-of-line)
;;                                             (skip-chars-backward " \t")
;;                                             (point))))
;;                                4)) ; regions are concatenated with "..."
;;                           ;; There is no context except the char itself.
;;                           (t 1))))
;;                   ((< 0 pos))
;;                   (win (frame-root-window frame))
;;                   (buffer (window-buffer win)))
;;         (move-overlay show-paren--overlay pos (1+ pos) buffer)
;;         (overlay-put show-paren--overlay 'priority show-paren-priority)
;;         (overlay-put show-paren--overlay 'face overlay-face))))

;;   :custom
;;   (show-paren-context-when-offscreen 'child-frame)
;;   :config
;;   ;; Make border for context in child frame a little bit more prominent
;;   (setf (alist-get 'child-frame-border-width
;;                    show-paren--context-child-frame-parameters)
;;         2)
;;   (setf (alist-get 'font
;;                    show-paren--context-child-frame-parameters)
;;         (emaxx-font-name))
;;   (advice-add 'show-paren--show-context-in-child-frame
;;               :after #'emaxx--show-paren-tune-child-frame-context)
;;   (show-paren-mode t))

;;; yasnippet
(use-package yasnippet :ensure t :config (yas-reload-all))

(add-hook 'prog-mode-hook #'prog-mode-init)
;; (lambda () (interactive)
;;   (show-paren-mode t)         ;show matching parentheses
;;   (setq show-trailing-whitespace 1)
;;   ;; Electric pair: automatically close parenthesis, curly brace etc.
;;   (setq electric-pair-open-newline-between-pairs t)
;;   (electric-pair-mode)
;;   (display-line-numbers-mode t)
;;   (tab-line-mode t)
;;   (editorconfig-mode t)
;;   (define-key prog-mode-map (kbd "RET") 'newline-and-indent)
;;   ;; (fci-mode 1) ; FIXME: fill-column-indicator (aka fci-mode) breaks company mode integration
;;   ;; Font lock mode in prog-mode
;;   (font-lock-add-keywords
;;    nil
;;    '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
;;      ("\\<\\(TBD\\):" 1 font-lock-warning-face prepend)
;;      ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)))
;;   (setq font-lock-maximum-decoration t
;;         jit-lock-chunk-size 5000
;;         jit-lock-context-time 0.2
;;         jit-lock-defer-time .1
;;         jit-lock-stealth-nice 0.2
;;         jit-lock-stealth-time 5
;;         jit-lock-stealth-verbose nil
;;         )
;;   (font-lock-mode t)
;;   (jit-lock-mode t)
;;   ;; enable yasnippet minor mode in prog-mode
;;   (yas-minor-mode t)))

(define-key prog-mode-map (kbd "RET") 'newline-and-indent)
(font-lock-add-keywords
 nil
 '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
   ("\\<\\(TBD\\):" 1 font-lock-warning-face prepend)
   ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)))

;;; Mouse selection
(use-package select
  :ensure nil
  :custom
  (select-enable-clipboard t))          ;selection is copied to clipboard

;;; http://www.reddit.com/r/emacs/comments/30g5wo/the_kill_ring_and_the_clipboard/
(setq save-interprogram-paste-before-kill t)

;; enshorten yes/no answers
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Delete selection when typing
(delete-selection-mode t)

;;; Let me scroll the buffer while searching, without exiting the search.
;;; This allows for using C-l during isearch.
(when (boundp 'isearch-allow-scroll)
  (setq isearch-allow-scroll t))

;;; C-x C-b = ibuffer (better than list-buffers)
(bind-key "C-x C-b" #'ibuffer)

;; When you visit a file, point goes to the last place where it was when you previously visited the same file
(use-package saveplace
  :config
  (save-place-mode t)
  ;; (add-hook 'after-save-hook 'my-save-place-alist-to-file)) ; after-save-hook seems to be the best place for this
  :custom
  (save-place-file (locate-user-emacs-file "places" ".emacs-places"))
  )

(use-package saveplace-pdf-view
  :after (:any doc-view pdf-tools)
  :demand t)

;;; Zoom
(use-package default-text-scale
  :bind
  ("C-+" . #'default-text-scale-increase)
  ("C--" . #'default-text-scale-decrease)
  ("C-<mouse-4>" . #'default-text-scale-increase)
  ("C-<mouse-5>" . #'default-text-scale-decrease))

;;; Expand selected region semantically
(use-package expand-region
  :bind
  (("C-=" . #'er/expand-region)
   ("C-M-=" . #'er/contract-region)))

;;; Move regions up and down (from https://www.emacswiki.org/emacs/MoveRegion)
(defun move-region (start end n)
  "Move the current region from START to END up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current region from START to END up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current region from START to END down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(bind-key "M-<up>" #'move-region-up)
(bind-key "M-<down>" #'move-region-down)

;;; Replace standard welcome window
;;; with more funny dashboard
;;; from dashboard/dashboard-hakernews
;;; package

;;; Remove welcome message
(setq inhibit-startup-message t)

(use-package dashboard-hackernews :ensure t)

(use-package dashboard
  :after (all-the-icons dashboard-hackernews) ;; helm-system-packages
  :init
  (dashboard-setup-startup-hook)

  :custom
  (dashboard-banner-logo-title "Let's get stuff done!")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-navigator t)
  ;;   (dashboard-navigator-buttons '((("⤓" " Install system package" " Install system package" (lambda (&rest _) (helm-system-packages))))))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((projects . 10)
                     (recents . 15)
                     (hackernews . 5))))

;; display PDF
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width) ; fit-page
  (setq pdf-view-use-scaling t)
  ;; (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(use-package google-translate
  :config
  (global-set-key (kbd "<f12>") 'google-translate-at-point)
  (global-set-key (kbd "S-<f12>") 'google-translate-at-point-reverse)
  (global-set-key (kbd "M-<f12>") 'google-translate-smooth-translate)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "uk")
  )

(use-package google-translate-smooth-ui
  :after google-translate)

;;
;; Themes and Appearance
;;

(load-theme 'material t) ;; load material theme
;; TODO: need to do smth with themes
;; (load-theme 'afternoon-green t)

;; use additional icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :autoload (all-the-icons-octicon
             all-the-icons-octicon-family))

;; use emoji
(use-package emojify
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-emojify-mode))

;;;
;;; Code formatting
;;;
(use-package format-all
  :hook ((format-all-mode . format-all-ensure-formatter)
         (prog-mode . format-all-mode))
  :custom
  (format-all-default-formatters
   `(("Assembly" asmfmt)
     ("ATS" atsfmt)
     ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex)
     ("C" clang-format)
     ("C#" csharpier)
     ("C++"
      (clang-format ,(concat "-style=file"))) ;; ...=file:some_dir/.clang-format"
     ("Cabal Config" cabal-fmt)
     ("Clojure" zprint)
     ("CMake" cmake-format)
     ("Crystal" crystal)
     ("CSS" prettier)
     ("Cuda" clang-format)
     ("D" dfmt)
     ("Dart" dart-format)
     ("Dhall" dhall)
     ("Dockerfile" dockfmt)
     ("Elixir" mix-format)
     ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp)
     ("Erlang" efmt)
     ("F#" fantomas)
     ("Fish" fish-indent)
     ("Fortran Free Form" fprettify)
     ("GLSL" clang-format)
     ("Go" gofmt)
     ("GraphQL" prettier)
     ("Haskell" brittany)
     ("HTML" html-tidy)
     ("HTML+EEX" mix-format)
     ("HTML+ERB" erb-format)
     ("Java" clang-format)
     ("JavaScript" prettier)
     ("JSON" prettier)
     ("JSON5" prettier)
     ("Jsonnet" jsonnetfmt)
     ("JSX" prettier)
     ("Kotlin" ktlint)
     ("LaTeX" latexindent)
     ("Less" prettier)
     ("Literate Haskell" brittany)
     ("Lua" lua-fmt)
     ("Markdown" prettier)
     ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format)
     ("OCaml" ocp-indent)
     ("Perl" perltidy)
     ("PHP" prettier)
     ("Protocol Buffer" clang-format)
     ("PureScript" purty)
     ("Python" black)
     ("R" styler)
     ("Reason" bsrefmt)
     ("ReScript" rescript)
     ("Ruby" rufo)
     ("Rust" rustfmt)
     ("Scala" scalafmt)
     ("SCSS" prettier)
     ("Shell" shfmt)
     ("Solidity" prettier)
     ("SQL" sqlformat)
     ("Svelte" prettier)
     ("Swift" swiftformat)
     ("Terraform" terraform-fmt)
     ("TOML" prettier)
     ("TSX" prettier)
     ("TypeScript" prettier)
     ("V" v-fmt)
     ("Verilog" istyle-verilog)
     ("Vue" prettier)
     ("XML" html-tidy)
     ("YAML" prettier)
     ("Zig" zig)
     ("_Angular" prettier)
     ("_Caddyfile" caddy-fmt)
     ("_Flow" prettier)
     ("_Gleam" gleam)
     ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt)
     ("_Snakemake" snakefmt))))

;;;
;;; File saving and opening
;;;

;; Warn when opening files bigger than 100MB (use nil to disable it entirely)
(setq large-file-warning-threshold 100000000)

;; Propose vlf (Very Large File) as a choice when opening large files
;; (otherwise one can open a file using M-x vlf):
(use-package vlf-setup
  :ensure vlf
  :defer t)

;;; Disable backup files (e.g. file~)
(defun no-backup-files ()
  "Disable creation of backup files."
  (interactive)
  (setq make-backup-files nil))

(unless emaxx-backup-files
  (no-backup-files))

;;; delete trailing whitespaces on save
;; (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)

;;; font-lock and code highlight
;;; NOTE: adopted from exordium
;;; emacs config set
(use-package symbol-overlay
  :diminish
  :commands (symbol-overlay-map-help)
  :functions (emaxx--extract-font-lock-keywords)
  :init
  (let ((form '(add-hook 'prog-mode-hook #'symbol-overlay-mode)))
    (if (boundp 'prog-mode-hook)
        (eval form)
      (eval-after-load 'prog-mode `,form)))
  (let ((form '(add-hook 'helpful-mode-hook #'symbol-overlay-mode)))
    (if (boundp 'helpful-mode-hook)
        (eval form)
      (eval-after-load 'helpful-mode `,form)))

  ;; Remove temporary highlighting from Emacs Lisp and Lisp keywords.  N.B.,
  ;; This matches only some of keywords, more basic for example `defun',
  ;; `progn', or `cl-defmacro', but not constructs like `if', `setq' or `let'.
  ;; The latter will still be highlighted.
  (defun emaxx--extract-font-lock-keywords (keywords)
    "Extract regexp from `font-lock' style KEYWORDS."
    (with-temp-buffer
      (insert (caar keywords))
      (when-let* ((begin (1+ (point-min)))
                  ((< begin (point-max))))
        (goto-char (1+ (point-min)))
        (re-search-forward (rx "\\_>") nil t)
        (when (< begin (point))
          (concat "\\`" (buffer-substring-no-properties begin (point)))))))

  (require 'lisp-mode)
  (defconst emaxx--symbol-overlay-ignore-keywords-el
    (emaxx--extract-font-lock-keywords lisp-el-font-lock-keywords-1))
  (defconst emaxx--symbol-overlay-ignore-keywords-cl
    (emaxx--extract-font-lock-keywords lisp-cl-font-lock-keywords-1))

  (defun emaxx--symbol-overlay-ignore-function-el (symbol)
    "Determine whether SYMBOL should be ignored (Emacs-Lisp Language)."
    (when emaxx--symbol-overlay-ignore-keywords-el
      (string-match-p emaxx--symbol-overlay-ignore-keywords-el symbol)))

  (defun emaxx--symbol-overlay-ignore-function-cl (symbol)
    "Determine whether SYMBOL should be ignored (Lisp Language)."
    (when emaxx--symbol-overlay-ignore-keywords-cl
      (string-match-p emaxx--symbol-overlay-ignore-keywords-cl symbol)))

  :bind
  ("C-c C-SPC" . #'symbol-overlay-put)

  :config
  (unless (get 'symbol-overlay-map
               'emaxx-original-value)
    (put 'symbol-overlay-map
         'emaxx-original-value
         (copy-keymap symbol-overlay-map)))

  ;; Add ts-modes handling.  Do it here, as it can't be referred to a definiens
  ;; in a definiendum.  The reason being that, when using :custom, the
  ;; unevaluated definiendum is installed for setting when the relevant
  ;; defcustom is evaluated.  The latter happens when feature is loaded.  This
  ;; is nice as it allows to install a custom standard value for variable,
  ;; without a need to set it to the standard.  However, at such a point
  ;; there's no standard value for the variable (the definiens).
  (dolist (elt (append
                '((emacs-lisp-mode . emaxx--symbol-overlay-ignore-function-el)
                  (lisp-interaction-mode . emaxx--symbol-overlay-ignore-function-el)
                  (lisp-mode . emaxx--symbol-overlay-ignore-function-cl))
                (delq
                 nil (mapcar
                      (lambda (elt)
                        (when-let* ((mode (car elt))
                                    (ts-mode (intern (replace-regexp-in-string
                                                      (rx "-mode" string-end)
                                                      "-ts-mode"
                                                      (symbol-name mode))))
                                    ((not (eq ts-mode mode)))
                                    ((fboundp ts-mode))
                                    ((not (assq ts-mode
                                                symbol-overlay-ignore-functions))))
                          (cons ts-mode (cdr elt))))
                      symbol-overlay-ignore-functions))))
    (unless (assq (car elt) symbol-overlay-ignore-functions)
      (push elt symbol-overlay-ignore-functions))))
;;; look-and-feel.el ends here.
