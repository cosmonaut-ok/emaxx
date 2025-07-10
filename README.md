# Cheat sheet

* select region then `M-up/M-down` to move region up/down
* `C-=/C-M-=` -- expand/unexpand region semantically
* `C-x C-\` goto-last-change; `C-x C-/` goto-last-change-reverse
* `M-x` -> `insert-current-time` or `insert-current-date-time`
* `M-x` -> `copy-all` - Copy the entire buffer to the clipboard
* `M-x` -> `kill-all-buffers` - yep
* `C-S-q` or `M-x` -> `unfill-paragraph` - Take a multi-line paragraph and make it into a single line of text.

## Helm
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

# Modules
0  init-lib.el
0  init-linum.el
0  _init-latex.el
0  _awesome-tab
0  _linum-and-linum-relative (https://github.com/coldnew/linum-relative)
0  _line-reminder https://github.com/emacs-vs/line-reminder
0  _artist-mode https://www.lysator.liu.se/~tab/artist/
0  https://doxymacs.sourceforge.net/
