SYS_PACKAGES_REQUIRED := libtext-multimarkdown-perl # for multimarkdown command
SYS_PACKAGES_REQUIRED += clangd-19		    # C/C++/ObjC language server
SYS_PACKAGES_REQUIRED += bear			    # to generate compile_commands.json

PIP_PACKAGES_REQUIRED := autotools-language-server

HERE := ${PWD}

EEVAL ?= /usr/bin/env emacs -Q --batch --eval

recompile:
	$(EEVAL) '(byte-recompile-directory "$(HERE)")'

clean:
	rm -rf eln-cache ielm-history.eld *~

bootstrap: init.el
	$(EEVAL) '(load "$(HERE)/bootstrap.el")'

sys-deps:
	sudo apt-get install -y -qq $(SYS_PACKAGES_REQUIRED)

.PHONY: init.el
init.el:
	@echo '(defconst const/self-dir (file-name-directory load-file-name))' > init.el
	@echo '(load (concat const/self-dir "" "configure-repos.el"))' >> init.el
	@echo '(load (concat const/self-dir "rc/" "rc.el"))' >> init.el

.pip-deps:
	pip install $(PIP_PACKAGES_REQUIRED)
