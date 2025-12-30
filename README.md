# EMAXX (Like [LEXX](https://www.imdb.com/title/tt0115243/))

This is an advanced emacs configuration allowing to work with `C/C++/C#` (including `autoconf/automake`), `python`, `javascript/typescript` (including `vue.js` framework), `groovy/jenkinsfile`, `emacs lisp`, `unix shell` and `powershell`, declarative files, like `xml/json/yaml/toml/hcl`, as well as CI, configuration management and cloud configurataion tools `ansible`, `docker/Dockerfile/docker-compose`, `terraform`, `kubernetes`, `jenkins` and `terraform`. Markup languages, like `markdown` and `LaTeX` are also suppoeted.

This configuration created for my personal use and distributed `as is` under terms of `unlicense` (Public Domain-like). Some code for this configuration adopted from [exordium](https://github.com/emacs-exordium/exordium.git). Thanks to creators for such beautiful config.

## Cheat sheet

* select region then `M-up/M-down` to move region up/down
* `C-=/C-M-=` -- expand/unexpand region semantically
* `C-x C-\` goto-last-change; `C-x C-/` goto-last-change-reverse
* `M-x` -> `insert-current-time` or `insert-current-date-time`
* `M-x` -> `copy-all` - Copy the entire buffer to the clipboard
* `M-x` -> `kill-all-buffers` - yep
* `C-S-q` or `M-x` -> `unfill-paragraph` - Take a multi-line paragraph and make it into a single line of text.

### Helm
* `M-x` -- Execute command with helm.
* `M-y` -- Remap standard: Yank with helm.
* `C-x b` -- Remap standard: Switch buffer with helm.
* `C-x C-f` -- Remap standard: Find file with helm.
* `C-x C-r` -- Open recent file with Helm (see also `init-ido.el`).
* `C-h b` -- Describe keybindings using Helm.
* `C-S-r` -- Search with ripgrep: in current project root.
* `C-S-d` -- Search with Ag: ask for directory first. See also`init-helm-porojectile.el`.
* `C-S-s` -- Helm occur
* `C-x c g` -- Helm Google suggest.
* `C-c C-p` -- Edit helm ag/grep/occur etc. search results (after exporting/saving them)
* `C-c h` -- helm-projectile
* `C-c H` -- helm-projectile-switch-project
* `C-c M-h` -- helm-projectile-switch-project
* `C-S-a` -- helm-projectile-ag
* `C-S-r` -- helm-projectile-rg

## Installation

NOTE: tested only on **Debian GNU Linux 13** distributive.

1. clone this repo to `~/.emacs.d` directory
1. run `make deps`
2. ...
3. just launch the `emacs` command
