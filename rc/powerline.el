;;; powerline.el --- powerline module configuration for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package powerline
  :if (display-graphic-p)
  :autoload (powerline-set-selected-window))

;;; Our Powerline theme.

(defun emaxx-powerline-buffer-face (active)
  "Return the face to use for the buffer name depending on whether it is ACTIVE."
  (cond ((not active)
         ;; Gray
         'emaxx-powerline-inactive3)
        (t
         ;; Purple
         'emaxx-powerline-active3)))

(defun emaxx-powerline-git (face)
  "Return the git branch string to display using the specified FACE.
This includes a branch icon."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     ;; branch icon
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 0.9
                                 :family ,(all-the-icons-octicon-family)
                                 :background ,(face-background face)))
     ;; branch name
     (format " %s" branch))))

(defun emaxx-powerline-svn (face)
  "Return the SVN revision number string to display using the specified FACE.
This includes a branch icon."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     ;; branch icon
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 0.9
                                 :family ,(all-the-icons-octicon-family)
                                 :background ,(face-background face)))
     ;; revision number
     (format " %s" revision))))

(defun emaxx-powerline-vc (face)
  "Return the version control string to display using the specified FACE.
Return nil if the current buffer is not under version control"
  (when vc-mode
    (cond
     ((string-match "Git[:-]" vc-mode)
      (emaxx-powerline-git face))
     ((string-match "SVN-" vc-mode)
      (emaxx-powerline-svn face))
     (t
      ;; fallback
      (format "%s" vc-mode)))))

(defun emaxx-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (when (eq emaxx-powerline-theme :wave)
    (setq-default powerline-default-separator 'wave))
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'emaxx-powerline-active1 'emaxx-powerline-inactive1))
             (face2 (if active 'emaxx-powerline-active2 'emaxx-powerline-inactive2))
             (face3 (if active 'emaxx-powerline-active3 'emaxx-powerline-inactive3))
             (separator-left (intern
                              (format "powerline-%s-%s"
                                      powerline-default-separator
                                      (car powerline-default-separator-dir))))
             (separator-right (intern
                               (format "powerline-%s-%s"
                                       powerline-default-separator
                                       (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw "%*" face3 'l)
                        (powerline-raw "%b " face3 'l)
                        (when (and (boundp 'which-func-mode) which-func-mode)
                          (powerline-raw which-func-format face3 'l))
                        (funcall (if (eq emaxx-powerline-theme :wave)
                                     separator-right
                                   separator-left)
                                 face3 mode-line)
                        ;;(funcall separator-left face3 face2)
                        (when (boundp 'erc-modified-channels-object)
                          (powerline-raw erc-modified-channels-object face1 'l))
                        (powerline-major-mode face1 'l)
                        (powerline-process face1)
                        (powerline-minor-modes face1 'l)
                        (powerline-narrow face1 'l)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 face2)
                        (if window-system
                            (powerline-raw (emaxx-powerline-vc face2) face2)
                          (powerline-vc face2 'r))))
             (rhs (list (powerline-raw global-mode-string face2 'r)
                        (funcall separator-right face2 face1)
                        (powerline-raw "%4l" face1 'l)
                        (powerline-raw ":" face1 'l)
                        (powerline-raw "%3c" face1 'r)
                        (funcall separator-right face1 mode-line)
                        (powerline-raw " ")
                        (powerline-raw "%6p" nil 'r))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

(defun display-powerline ()
  "(Re)Displays Powerline."
  (powerline-set-selected-window)
  ;; (emaxx-powerline-theme)
  (powerline-default-theme)
  (redraw-display))

;; Depending on the preferences, either display powerline immediately or after
;; a number of seconds of idle time. This is a fix for Emacs crashing on some
;; environments.
;; (if (eq emaxx-display-powerline-after-idle-time 0)
;;     (emaxx-powerline-theme)
;;   (run-with-idle-timer emaxx-display-powerline-after-idle-time nil
;;                        #'display-powerline))
(display-powerline)
;;; powerline.el ends here.
