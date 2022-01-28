;; Load with:
(load-file "~/repos/tb-keycast/tb-keycast.el")
;; or this:
(add-to-list 'load-path "~/repos/tb-keycast")
(require 'tb-keycast)
;; or read README file for more installation details.

;; Enable.
(tb-keycast-mode 1)
;; Disable
(tb-keycast-mode -1)

;; Test on different build in themes.
(load-theme 'modus-operandi t)
(load-theme 'modus-vivendi t)

;; Default, aligned to right with 32 min width.
(setq tb-keycast-min-width 32)
(setq tb-keycast-align-right-p t)
(setq tb-keycast-format
      '((" %s " tb-keycast--key :inherit mode-line-highlight :weight bold)
        (" %s " tb-keycast--cmd :slant italic)
        ("x%d " (lambda () (if (> tb-keycast--count 1) tb-keycast--count)))))

;; One face to all values, counter is always visible.
(setq tb-keycast-format
      '((" %s x%d %s "
         (tb-keycast--key tb-keycast--count tb-keycast--cmd)
         :inherit mode-line-highlight :weight bold)))

;; Just keys and counter with dynamic width and some static text.
(setq tb-keycast-min-width 0)
(setq tb-keycast-format
      '((" Key: %s x%d "
         (tb-keycast--key tb-keycast--count)
         :inherit mode-line-highlight :weight bold)))

;; Shadow mode alligned to left.
(setq tb-keycast-align-right-p nil)
(setq tb-keycast-format
      '((" %s " tb-keycast--key :inherit shadow :weight bold)
        ("x%d " tb-keycast--count :inherit shadow)
        ("%s " tb-keycast--cmd :inherit shadow :slant italic)))

;; No tabs, just tb-keycast.
(setq tab-bar-format nil)
(tb-keycast-mode 1)

;; Stop ignoreing regular typing.
(setq tb-keycast-ignore '(minibuffer))
