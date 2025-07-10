HERE := ${PWD}

EEVAL ?= /usr/bin/env emacs -Q --batch --eval

DEPS := $(shell find rc/deps-loaders/ -type f -exec echo '"{}"' \; | sort -n)

deps:
	$(EEVAL) '(dolist (v `($(DEPS))) (load  (locate-user-emacs-file v))))'

.package-deps:
	$(EEVAL) '(load (locate-user-emacs-file "rc/deps-loaders/05-package-deps-loader.el"))'

.lsp-deps:
	$(EEVAL) '(and (load (locate-user-emacs-file "rc/deps-loaders/05-package-deps-loader.el")) (load (locate-user-emacs-file "rc/deps-loaders/07-lsp-servers-loader.el")))'

recompile:
	$(EEVAL) '(byte-recompile-directory "$(HERE)")'

clean:
	rm -rf eln-cache ielm-history.eld *~

mrproper:
	rm -rf 3rd-party auto-save-list el-get eln-cache elpa emojis places projectile-bookmarks.eld\
		recentf request snippets newsticker transient url ielm-history.eld elpa-*\
		company-statistics-cache.el tramp emaxx-custom.el
	find . -type f -name '*~' -delete
