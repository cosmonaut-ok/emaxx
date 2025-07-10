;;; cpp.el --- c/c++ modes configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package cc-mode
  :init
  (defun call-interactivaly-compile ()
    (interactive)
    (setq-local compilation-read-command nil)
    (call-interactively 'compile))
  (defun cpp-highlight-dead-code ()
    "Highlight c/c++ #if 0 #endif macros."
    (let ((color (face-background 'shadow)))
      (setq cpp-known-face 'default)
      (setq cpp-unknown-face 'default)
      (setq cpp-known-writable 't)
      (setq cpp-unknown-writable 't)
      (setq cpp-edit-list `(("0" (background-color . ,color) default both)
                            ("1" default (background-color . ,color) both)))
      (cpp-highlight-buffer t)))
  (defun c-modes-init ()
    (lsp)
    (lsp-ui-mode t)

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
    ;; (c-set-style "bsd")

    ;; (setq
    ;;  c-default-style "bsd"
    ;;  c-basic-offset 2
    ;;  tab-width 2
    ;;  indent-tabs-mode nil)

    ;; "Highlight dead code blocks."
    (cpp-highlight-dead-code)
    (add-hook 'after-save-hook #'cpp-highlight-dead-code 'append 'local)
    )

  :hook (c-mode-common . c-modes-init) ;; (c-mode c++-mode)
	 
  :config
  ;; Open a header file in C++ mode by default
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  ;; (define-key c-mode-map  [(control tab)] 'company-complete)
  ;; (define-key c++-mode-map  [(control tab)] 'company-complete)
  ;; (setq
  ;;  c-default-style "bsd"
  ;;  c-basic-offset 2
  ;;  tab-width 2
  ;;  indent-tabs-mode nil)

  :custom
  (c-default-style "bsd")
  (c-basic-offset 2)
  (tab-width 2)
  (indent-tabs-mode nil)
  :bind (("<f5>" . call-interactivaly-compile))
  )

;;; Don't show the abbrev minor mode in the mode line
(diminish 'abbrev-mode)

;; ;; dap-mode -- emacs client/library for Debug Adapter Protocol is a wire protocol
;; ;; for communication between client and Debug Server. It’s similar to the LSP
;; ;; but provides integration with debug server.
;; (use-package dap-mode
;;   :init (defun dap-init()
;;    (call-interactively #'dap-hydra))
;;   (setq dap-auto-configure-features '(sessions locals controls tooltip))
;;   :hook (dap-stopped . dap-init)
;;   )
;;   ;; (add-hook 'dap-stopped-hook
;;   ;;         (lambda (arg) (call-interactively #'dap-hydra)))

;; cwarn -- is an Emacs minor mode that highlights potentially problematic C and C++ code constructs
(use-package cwarn
  :ensure t
  :hook ((c-mode c++-mode) . cwarn-mode))

;; modern (C++20) C++ font lock
(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

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
;; ;;    (let ((file (file-name-nondirectory buffer-file-name)))
;; ;;      (format "%s -o %s %s"
;; ;;        (if (equal (file-name-extension file) "cpp") "g++" "gcc")
;; ;;        (file-name-sans-extension file)
;; ;;        file)))
;; ;;     (compile compile-command)))
;; ;; (global-set-key [f9] 'code-compile)

(use-package clang-format
  :ensure t
  :config
  (global-set-key (kbd "C-c i") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer))

;; arduino section
(use-package arduino-cli-mode
  :ensure t
  ;; :hook arduino-mode
  ;; :mode "\\.ino\\'"
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))

(use-package arduino-mode
  :mode "\\.ino\\'"
  :init
  (defun arduino-mode-init()
    (lsp)
    (lsp-ui-mode t))
  :ensure t
  :hook (arduino-mode . arduino-mode-init))

;; (use-package company-arduino :ensure t :after company
;;   :config
;;   (add-to-list 'company-backends 'company-arduino)) ;FIXME: requires irony, but irony is broken (from elpa/melpa)
(use-package lsp-arduino :after lsp)    ;3rd party

;;;
;;; GDB part
;;;
;; -------------- -------------------------------------------------------
;; Key            Definition
;; -------------- -------------------------------------------------------
;; F5             Step into (s)
;; F6             Next (n)
;; F7             Run to the end of the function
;; F8             Run
;;
(require 'gdb-mi)
(require 'gud)

;; Show main source buffer when using GDB.
(setq gdb-show-main t)

;;; Highlight the current line in the source window.
(defconst gdb-highlight-face 'highlight
  "Face to use for highlighting the current line.")

(defvar gud-overlay
  (let ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face gdb-highlight-face)
    ov)
  "Overlay variable for GUD highlighting.")

(defun emaxx--gud-highlight (true-file _line)
  "Highlight the current line in TRUE-FILE."
  (let ((ov gud-overlay)
        (bf (gud-find-file true-file)))
    (with-current-buffer bf)
    (move-overlay ov (line-beginning-position) (line-end-position)
                  (current-buffer))))
(advice-add 'gud-display-line :after #'emaxx--gud-highlight)

(defun gud-kill-buffer ()
  "Kill GUD buffer by deleting overlay."
  (if (eq major-mode 'gud-mode)
      (delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook #'gud-kill-buffer)
;;; Keep the current line in sync with the point and in the center of the
;;; buffer. Otherwise the current line may disappear from the buffer as you step
;;; into the code. I don't know why this is not the default.
(defun emaxx--gud-display-line-centered (_true-file line)
  "Center the current LINE in the source code window."
  (when gud-overlay-arrow-position
    (with-selected-window (gdb-display-buffer
                           (gdb-get-buffer 'gdb-assembler-buffer))
                                        ; (marker-buffer gud-overlay-arrow-position)
      (save-restriction
        ;; Compiler-happy equivalent to (goto-line (ad-get-arg 1))
        (goto-char (point-min))
        (forward-line (1- line))
        (recenter)))))
(advice-add 'gud-display-line :after #'emaxx--gud-display-line-centered)

(defun gdb-few-windows ()
  "Slit the current frame into 3 windows.
Windows created are gdb command line, source code, and program IO."
  (interactive)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil ( / ( * (window-height) 3) 4)))
        (win2 (split-window nil ( / (window-height) 3))))
    ;; IO
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win1)
    ;; Source code or current buffer if not found.
    (set-window-buffer
     win2
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put the buffer list in window if we cannot find a source file...
         (list-buffers-noselect))))
    ;; Give focus to the comint window.
    (set-window-dedicated-p win0 t)
    (select-window win0)))

;;; Keys
;; Suppress compiler warnings, like in
;; https://github.com/emacs-mirror/emacs/blob/a0f8fb8/lisp/progmodes/gud.el#L59-L76
(eval-when-compile
  (declare-function gud-finish    "gud" (arg))
  (declare-function gud-cont      "gud" (arg))
  (declare-function gud-next      "gud" (arg))
  (declare-function gud-step      "gud" (arg)))
(bind-key "<f5>" #'gud-step gud-minor-mode-map)
(bind-key "<f6>" #'gud-next gud-minor-mode-map)
(bind-key "<f7>" #'gud-finish gud-minor-mode-map)
(bind-key "<f8>" #'gud-cont gud-minor-mode-map)

(use-package helm-make
  :ensure t
  :after helm)
;;; cpp.el ends here.
