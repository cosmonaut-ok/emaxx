;;; custom.el --- customizations for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;
;; define customs here (if any)
;; e.g. (defcustom ...)
;;

;;; Code:
(defgroup emaxx nil
  "Customize your Emacs configuration."
  :group 'local)

;;; UI -- section

(defcustom emaxx-preferred-fonts
  '(("Consolas"  . 110)
    ("Monaco"    . 110)
    ("Monospace" . 110)
    ("Mono"      . 110)
    ("Go Mono" . 110)
    ("DejaVu Sans Mono" . 110)
    ("Linux Libertine Mono O" . 110)
    ("Andale Mono" . 110)
    ("Nimbus Mono PS" . 110)
    ("Noto Sans Mono" . 110)
    ("github-octicons" . 110)
    ("FreeMono"  . 110)
    ("Roboto" . 110)
    ("Terminus"  . 110)
    ("Roboto Condensed" . 110)

    )
  "List of preferred fonts/sizes to use.
The list is in decreasing order of preference.  We will use the
first one that is available on the local machine.  It is an alist
of pairs (FONT-NAME . FONT-SIZE).  If nil, we don't set any font.
Note that you can get the list of available font names by
evaluating `font-family-list'."
  :group 'emaxx
  :type  'sexp)

(defcustom emaxx-fill-column-number 120
  "Determines the maximum width of text lines in a buffer."
  :group 'emaxx
  :type  'integer)

(defcustom emaxx-backup-files t
  "Backup editing files or not."
  :group 'emaxx
  :type  'boolean)

(defcustom emaxx-lsp-clangd-executable ["clangd-19"
                                        "clangd-18"
                                        "clangd-17"
                                        "clangd-16"
                                        "clangd-15"
                                        "clangd-14"
                                        "clangd-13"
                                        "clangd"]
  "List of executable names to search for to run clangd.
Default is to choose the first that is found via `executable-find'."
  :group 'emaxx
  :risky t
  :type 'emaxx-string-vector)

(defcustom emaxx-lsp-clangd-args '("-j=4"
                                   "--background-index"
                                   "--log=error"
                                   "--clang-tidy")
  "Extra arguments for the clangd executable."
  :group 'emaxx
  :risky t
  :type '(repeat string))

(defcustom emaxx-powerline-theme :angle
  "Control the shape of Powerline separators, and other things.
Possible values are :angle, :wave"
  :group 'emaxx
  :type  'symbol)

(defcustom emaxx-helm-grep-ag-command
  (cond ((executable-find "ag")
         "ag --line-numbers -S --color --color-match '1;34' --nogroup %s -- %s %s")
        (t ;; Fall back to rg, just like `helm-grep' does
         "rg --color=always --colors='match:fg:blue' --smart-case --search-zip --no-heading --line-number %s -- %s %s"))
  "Default value for `helm-grep-ag-command', which see.
Unlike `helm', Emaxx prefers to use \"ag\" in case when it is
available, as the \"rg\" is used via package `helm-rg'.  In addition
colors are costomized. Ag is a part of silversearcher package
(e.g. debian `silversearcher-ag')."
  :group 'emaxx
  :type 'string)

;;;
;;; UI Faces
;;;

;;; Faces for our powerline theme. They are defined here and customized within
;;; each theme.

(defface emaxx-powerline-active1 '((t (:inherit mode-line)))
  "Powerline active face 1."
  :group 'emaxx)

(defface emaxx-powerline-active2 '((t (:inherit mode-line)))
  "Powerline active face 1."
  :group 'emaxx)

(defface emaxx-powerline-active3 '((t (:inherit mode-line)))
  "Powerline active face 3 (buffer name)."
  :group 'emaxx)

(defface emaxx-powerline-inactive1 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 1."
  :group 'emaxx)

(defface emaxx-powerline-inactive2 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 2."
  :group 'emaxx)

(defface emaxx-powerline-inactive3 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 3 (buffer name)."
  :group 'emaxx)
;;; custom.el ends here.
