;;; 98-yasnippet-latex-loader.el --- latex yasnippet snippets loader for Emaxx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defconst const/yas-latex--dir (expand-file-name (locate-user-emacs-file (file-name-concat "snippets" "latex-mode"))))

(defconst const/yas-latex--url-prefix "https://raw.githubusercontent.com/madsdk/yasnippets-latex/refs/heads/master/snippets/")

(defconst const/yas-latex--parents-name ".yas-parents")
(defconst const/yas-latex--ignore-filenames-as-triggers-name ".yas-ignore-filenames-as-triggers")
(defconst const/yas-latex--make-groups-name ".yas-make-groups")

(defconst const/yas-latex--snippets
  '(
    "abstract.yasnippet" "align.yasnippet" "alignstar.yasnippet" "array.yasnippet" "article.yasnippet" "beamer.yasnippet" "begin.yasnippet" "bib.yasnippet"
    "big.yasnippet" "bigop.yasnippet" "binom.yasnippet" "block.yasnippet" "bold.yasnippet" "case.yasnippet" "cha.yasnippet" "chastar.yasnippet" "cite.yasnippet"
    "coprod.yasnippet" "desc.yasnippet" "doc.yasnippet" "em.yasnippet" "enum.yasnippet" "eq.yasnippet" "eqs.yasnippet" "fig.yasnippet" "frac.yasnippet"
    "frame.yasnippet" "gls.yasnippet" "graphics.yasnippet" "href.yasnippet" "int.yasnippet" "it.yasnippet" "itd.yasnippet" "item.yasnippet" "label.yasnippet"
    "letter.yasnippet" "lim.yasnippet" "math.yasnippet" "matrix.yasnippet" "minipage.yasnippet" "par.yasnippet" "prod.yasnippet" "ref.yasnippet" "sc.yasnippet"
    "sec.yasnippet" "secstar.yasnippet" "ssub.yasnippet" "ssubstar.yasnippet" "sub.yasnippet" "subfig.yasnippet" "substar.yasnippet" "sum.yasnippet" "table.yasnippet"
    "tt.yasnippet" "url.yasnippet" "use.yasnippet" "verb.yasnippet"))

;;
;; install latex sippets
;;
(defun download-file (url download-path)
  (let ((download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      ;; we may have to trim the http response
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (write-file download-path))))

(make-directory const/yas-latex--dir t)

;; download .yas-parents
(download-file (file-name-concat const/yas-latex--url-prefix const/yas-latex--parents-name)
               (file-name-concat const/yas-latex--dir const/yas-latex--parents-name))

;; download .yas-ignore-filenames-as-triggers
(download-file (file-name-concat const/yas-latex--url-prefix const/yas-latex--ignore-filenames-as-triggers-name)
               (file-name-concat const/yas-latex--dir const/yas-latex--ignore-filenames-as-triggers-name))

;; download .yas-make-groups
(download-file (file-name-concat const/yas-latex--url-prefix const/yas-latex--make-groups-name)
               (file-name-concat const/yas-latex--dir const/yas-latex--make-groups-name))

(dolist (f const/yas-latex--snippets)
  (download-file (file-name-concat const/yas-latex--url-prefix f)
                 (file-name-concat const/yas-latex--dir f)))
;;; 98-yasnippet-latex-loader.el ends here.
