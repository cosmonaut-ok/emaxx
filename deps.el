;;
;; This is a declarative dependencies file
;; this file does nothing by itself
;; and should be used as include
;; in system-, pip-, gem-, go-
;; or packages dependencies loaders
;; (see rc/deps-loaders/*.el files)
;;

(defconst const/os-distro "debian")

(defconst const/sys-deps '(
                           ("debian"
                            ("libtext-multimarkdown-perl" ; for multimarkdown command
                             "clangd-19" "libclang-dev" "libedit-dev" "libzstd-dev" "libcurl4-openssl-dev" ; C/C++/ObjC language server and irony server
                             "bear"                       ; to generate compile_commands.json
                             "golang"                     ; required for arduino-language-server
			     "silversearcher-ag"	  ; for `ag` command used by helm
                             )
                            )
                           )
  )

(defconst const/pip-deps '("autotools-language-server" "python-lsp-server[all]" "ruff" "mypy" "ruff"))
(defconst const/gem-deps '("ruby-lsp" "rubocop"))
(defconst const/go-deps '("github.com/arduino/arduino-language-server@latest"))
(defconst const/npm-deps '("vscode-langservers-extracted" "dockerfile-language-server-nodejs"
			   "@vue/language-server" "yaml-language-server" "bash-language-server"
			   "@ansible/ansible-language-server"))

(defconst const/package-archives '(("elpa" . "http://tromey.com/elpa/")
                                   ("gnu" . "http://elpa.gnu.org/packages/")
                                   ("melpa" . "http://melpa.org/packages/")
                                   ("org" . "http://orgmode.org/elpa/")))

;;
;; declare package.el`s package list and el-get`s package list
;;
(defconst const/package-packages-list
  '(
    ;; beautify a bit
    ansible                             ;+
    poly-ansible                        ;+
    ansible-doc                         ;+
    all-the-icons                       ;+
    emojify                        ;add a bit of emoji+
    dashboard                      ;replace for default startup screen+
    dashboard-hackernews           ;add news to dashboard+
    material-theme                 ;use it as default theme+
    diminish ;hiding or abbreviation of the mode line displays (lighters) of minor-modes+
    exec-path-from-shell ;ensure environment variables inside Emacs look the same as in the user's shell+
    saveplace ;When you visit a file, point goes to the last place where it was when you previously visited the same file+
    saveplace-pdf-view                  ;+
    pdf-tools
    markdown-mode                       ;+
    arduino-mode                        ;+ TODO: replace with arduino2-mode (create) that is merge of arduino-mode and arduino-cli-mode
    arduino-cli-mode                    ;- TODO: replace with arduino2-mode (create) that is merge of arduino-mode and arduino-cli-mode
    company                             ;+
    company-ansible                     ;+
    ;; company-arduino                  ;- FIXME: requires irony, but irony is broken (from elpa/melpa)
    company-auctex                      ;- TODO: create configuration for LaTeX
    company-bibtex                      ;- TODO: create configuration for LaTeX
    company-c-headers                   ;+
    company-irony                       ;+
    company-math                        ;+
    company-quickhelp                   ;+
    company-quickhelp-terminal          ;+
    company-reftex                      ;- TODO: create configuration for LaTeX
    company-terraform                   ;+
    company-web                         ;+
    company-posframe                    ;+
    company-statistics                  ;+
    ;; web-related modes
    web-mode                            ;+
    typescript-mode                     ;+
    tide                                ;typescript development environment+
    js2-mode                            ;extended js-mode+
    vue3-mode                           ;for vue.js mode+
    ;; docker
    docker                              ;+
    dockerfile-mode                     ;+
    docker-compose-mode                 ;+
    ;; kubernetes
    k8s-mode                            ;+
    kubernetes-helm                     ;+
    ;; JSON
    json-mode                           ;+
    ;; yaml
    yaml-mode                           ;+
    hcl-mode				;+
    toml				;+
    toml-mode				;+
    ;; yasnippet
    yasnippet                           ;+
    yasnippet-snippets                  ;+
    terraform-mode                      ;for terraform tool+
    powershell                          ;
    ;; jenkins/groovy
    groovy-mode                         ;+
    jenkinsfile-mode                    ;+
    ;; flycheck mode
    flycheck                            ;+
    flycheck-pos-tip                    ;+
    flycheck-package                    ;+
    flycheck-relint                     ;+
    ;; python
    py-snippets                         ;+
    jinja2-mode                         ;+
    conda                               ;+
    flycheck-pyflakes                   ;+
    ;; golang
    go-mode
    ;; rust
    rust-mode
    ;; treemacs-related packages
    treemacs                            ;+
    treemacs-all-the-icons              ;+
    treemacs-magit                      ;+
    treemacs-projectile                 ;+
    ;; lsp-mode, related and special lsp modes
    lsp-mode                            ;+
    lsp-ui                              ;+
    lsp-treemacs                        ;+
    lsp-latex                           ;- TODO: create configuration for LaTeX
    ;; latex
    auctex
    biblio				; An extensible Emacs package for browsing and fetching references
    ;; various system modes
    systemd                             ;
    clang-format                        ;c/c++ language server+
    modern-cpp-font-lock                ;+
    ;; dap-mode                         ;Emacs client/library for Debug Adapter Protocol is a wire protocol
                                        ; for communication between client and Debug Server. It’s similar to
                                        ; the LSP but provides integration with debug server.
                                        ; TODO: implement it properly or remove as unneeded
    google-translate                    ;+
    ;;
    format-all                          ;+
    projectile                          ;+
    ;; helm-related packages
    helm                                ;+
    helm-projectile                     ;+
    helm-make                           ;+
    helm-pydoc                          ;+
    helm-lsp                            ;+
    helm-flycheck                       ;+
    helm-bibtex                         ;- TODO: create configuration for LaTeX
    helm-rg
    ;; user interface (look and feel)
    ;; fill-column-indicator            ;FIXME: breaks company mode
    default-text-scale                  ;+
    expand-region                       ;Expand selected region semantically+
    vlf                                 ;open very large files+
    goto-chg                            ;go to last change+
    helpful                             ;Enhanced help mode+
    symbol-overlay                      ;Enhanced font-lock mode+
    powerline                           ;enhanced modeline+
    ))

(defconst const/el-get-packages-list
  '(
    "company-lsp"
    ))

(defconst const/third-party-packages-list
  '(
    ("go-template-mode.el" "https://gist.githubusercontent.com/grafov/10985431/raw/dcd12037308446179f26f7d2ab2c034a1e995d2e/go-template-mode.el") ;-
    ("lsp-arduino.el" "https://raw.githubusercontent.com/mgrunwald/emacs-lsp-arduino/refs/heads/master/lsp-arduino.el") ;-
    ("helm-icons.el" "https://raw.githubusercontent.com/yyoncho/helm-icons/refs/heads/master/helm-icons.el"))) ;-
;;; deps.el ends here.
