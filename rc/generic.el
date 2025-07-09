(defconst const/self-dir (file-name-directory load-file-name))

;;
;; generic customizations (e.g. remove menubar etc.)
;;
(custom-set-variables
 '(menu-bar-mode -1)
 '(tool-bar-mode -1)
 '(scroll-bar-mode -1)
 ;; launch in full-screen mode
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 ;; ;; do not show startup screen
 ;; '(inhibit-startup-screen t)
 ;; '(inhibit-startup-message t)
 )

(use-package which-key :config (which-key-mode))

;;
;; configure saveplace package
;;
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

;; Set preferred encoding
(prefer-coding-system 'utf-8)

;;
;; configure dashboard and dashboard-related features
;;
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

(use-package dashboard-hackernews)

;;
;; configure themes and appearance
;;
(load-theme 'material t) ;; load material theme
;; TODO: need to do smth with themes
;; (load-theme 'afternoon-green t)

;;
;; ensure environment variables inside Emacs look the same as in the user's shell
;;
(use-package exec-path-from-shell
  :init
  (setenv "SHELL" "/bin/bash")
  :ensure t
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))
  :config
  (exec-path-from-shell-initialize))

;;
;; configure PDF display
;;
(use-package pdf-tools
  :config
   (pdf-tools-install)
   (setq-default pdf-view-display-size 'fit-width); fit-page
   (setq pdf-view-use-scaling t)
   ;; (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; (add-to-list 'org-file-apps 
;;              '("\\.pdf\\'" . (lambda (file link)
;;                                (org-pdfview-open link))))

;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

;; (add-hook 'TeX-after-compilation-finished-functions
;; 	  #'TeX-revert-document-buffer)

;;
;; configure all-the-icons
;;
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;;
;; configure emoji
;;
(use-package emojify
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-emojify-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package format-all
	     :hook (format-all-mode . format-all-ensure-formatter))

;; (setq format-all--user-args (concat "--style=file:" const/main-directory "/clang-formatter"))
(setq format-all-default-formatters
      `(("Assembly" asmfmt)
	("ATS" atsfmt)
	("Bazel" buildifier)
	("BibTeX" emacs-bibtex)
	("C" clang-format)
	("C#" csharpier)
	("C++"
	 (clang-format ,(concat "-style=file:" const/self-dir ".clang-format")))
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
	("_Snakemake" snakefmt)))

;; enshorten yes/no answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)
	    (display-line-numbers-mode t)
	    (tab-line-mode t)
	    (editorconfig-mode t)
	    (define-key prog-mode-map (kbd "RET") 'newline-and-indent)
	    ))


;; (defun cosmonaut/tabbar-buffer-groups () ;; customize to show all normal files in one group
;;   "Returns the name of the tab group names the current buffer belongs to.
;;     There are two groups: Emacs buffers (those whose name starts with '*', plus
;;     dired buffers), and the rest.  This works at least with Emacs v24.2 using
;;     tabbar.el v1.7."
;;   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
;;               ((eq major-mode 'dired-mode) "emacs")
;;               (t "user"))))
;; (setq tabbar-buffer-groups-function 'cosmonaut/tabbar-buffer-groups)


(defun treemacs-start ()
  "another nonce menu function"
  (interactive)
  (treemacs))

(tool-bar-add-item "spell" 'treemacs-start
		   'treemacs-start
		   :help   "Launch treemacs")
