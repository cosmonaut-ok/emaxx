;;; util.el --- various util functions for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Goto last change
(use-package goto-chg
  :bind
  (("C-x C-\\" . #'goto-last-change)
   ("C-x C-|" . #'goto-last-change-reverse)
   ("C-x C-/" . #'goto-last-change-reverse)))

;;; Insert date/time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' function.
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' function.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "Insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n"))

(defun insert-current-time ()
  "Insert the current time into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n"))

;;; Copy the whole buffer without changing the current position
(defun copy-all ()
  "Copy the entire buffer to the clipboard."
  (interactive)
  (without-restriction
    (clipboard-kill-ring-save (point-min) (point-max))))


;;; Buffers
(defun kill-all-buffers ()
  "Kill all buffers that are associated with a file."
  (interactive)
  (mapc (lambda (buff)
            (when (buffer-file-name buff)
              (kill-buffer buff)))
        (buffer-list)))

;;; Miscellaneous
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and make it into a single line of text.
REGION is t when called interactively and is passed to
`fill-paragraph', which see."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(bind-key "C-S-q" #'unfill-paragraph)
;;; util.el ends here.
