# Emacs Tab Bar Keycast

Emacs global minor mode showing last pressed key with corresponding
function name and repeat counter in `tab-bar-mode` line.  Ignore
typing, mouse drag, none key related minibuffer commands and other
random undefined bindings.

Can be aligned to the right (default) or left side of `tab-bar-mode`
line free space (after tabs).  Tabs may be hidden so you don't have to
use them to have `tb-keycast` status enabled.

**Variable `tab-bar-format` introduced in Emacs 28.1 is required.**

![demo](demo.gif)

## Installation and usage

Get `tb-keycast.el` file.

```shell
# You can clone entire repo
git clone git@github.com:ir33k/tb-keycast.git

# Or just get the file with one of those:
wget    https://raw.githubusercontent.com/ir33k/tb-keycast/master/tb-keycast.el
curl -O https://raw.githubusercontent.com/ir33k/tb-keycast/master/tb-keycast.el
```

Use `M-x load-file<RET>path_to/tb-keycast.el<RET>` or load with code:

```elisp
;; Put tb-keycast.el file to one of `load-path' dirs or add new path
;; to tb-keycast.el file in `load-path' list so Emacs know how to find
;; `tb-keycast' library.
(add-to-list 'load-path "~/path_dir_with_file")

;; Load with require or type: M-x load-library<RET>tb-keycast<RET>.
(require 'tb-keycast)
```

Toggle with `M-x tb-keycast-mode`.

## Howto

```elisp
(setq tab-bar-format nil)               ; Hide tabs if not used.
(setq tb-keycast-status-align 'left)    ; Align status to the left.

;; Disable min-width that makes `tb-keycast' always reserve some space
;; to avoid jumping from left to right.  But when tabs are hidden and
;; `tb-keycast' status is aligned to the left then min-width has little
;; to none sense and can be disabled.
(setq tb-keycast-status-min-width 0)

(tb-keycast-mode 1)                     ; Enable tb-keycast.
```

## Note

- Tested only on Emacs 29.0.
- Repeat counter does not work for `C-k (kill-line)`.
