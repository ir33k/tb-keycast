;;; tb-keycast.el --- Tab Bar Keycast -*- lexical-binding: t -*-

;; Version: 1.7
;; Keywords: keycast, tab-bar
;; Author: irek <mail@gumen.pl>
;; URL: https://github.com/ir33k/tb-keycast

;; This file is part of tb-keycast.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Global minor mode that shows in tab-bar-mode line last pressed keys
;; with corresponding function name and how many times was repeated.
;; It ignores regular typing, mouse drag, minibuffer commands not
;; executed by pressing keys and other random undefined bindings.


;;; Code:

(require 'tab-bar)

(defconst tb-keycast-version "1.7" "Version of `tb-keycast-mode'.")
(defvar tb-keycast--key "" "Last pressed key.")
(defvar tb-keycast--count 1 "Last key repeat counter.")
(defvar tb-keycast--cmd "" "Last used command name.")
(defvar tb-keycast--str "" "Last keycast status string.")

(defgroup tb-keycast nil "Tab Bar Keycast."
  :prefix "tb-keycast-" :group 'environment :version "28.1")

(defcustom tb-keycast-format
  '((" %s " tb-keycast--key :inherit mode-line-highlight :weight bold)
    (" %s " tb-keycast--cmd :slant italic)
    ("x%d " (lambda () (if (> tb-keycast--count 1) tb-keycast--count))))
  "Sorted list of items that produce `tb-keycast--str'.
Read doc of `tb-keycast--format-to-string' for details."
  :group 'tb-keycast :type '(repeat (cons string (cons variable (plist)))))

(defcustom tb-keycast-ignore '(typing  minibuffer)
  "List of ignored commands.
Possible key values: `typing', `minibuffer'."
  :group 'tb-keycast :type '(set (const :tag "Typing" typing)
                                 (const :tag "Minibuffer cmd" minibuffer)))

(defcustom tb-keycast-align-right-p t
  "Align tb-keycast status to right if value is not nil."
  :group 'tb-keycast :type 'boolean)

(defcustom tb-keycast-min-width 224
  "Minimal width (in pixels) that is always taken by keycast status.
This helps avoid jumping of status to left and right and to next
tab-bar line when there are more tabs.  Higher value makes
jumping less frequent.  To disable use 0."
  :group 'tb-keycast :type 'number)

(defun tb-keycast--format-to-string (format)
  "Transform format items to single formated string with faces.
FORMAT is a list of items.  Each item has format string \\(see
`format' for details\\) as first argument.  Second argument can
be a regular value, var name, list of values or var names,
function that evaluate to one of mentioned values.  Last argument
is optional face attributes, see Info node `(elisp) Faces' for
details.  Returned value is single string created by
concatenating formatting results of each item.  If item second
argument results in nil then item is ignored."
  (mapconcat
   (lambda (item)
     (seq-let (fmt var &rest face-attr) item
       (if (functionp var) (setq var (funcall var)))
       (if (or (eq var nil)
               (and (symbolp var) (eq (symbol-value var) nil)))
           nil
         (if (not (listp var)) (setq var (list var)))
         (setq var (seq-map (lambda (v)
                              (if (symbolp v) (symbol-value v) v))
                            var))
         (propertize (apply (apply-partially 'format fmt) var)
                     'face face-attr))))
   format))

(defun tb-keycast--update ()
  "Update key, cmd and count keycast vars to create `tb-keycast--str'."
  (when (and
         ;; Ignore undefined bindings.
         this-command
         ;; Ignore mouse move while drag to avoid lags.
         (not (string-match "^#" (format "%s" this-command)))
         ;; Maybe ignore regular typing.
         (or (not (seq-contains-p tb-keycast-ignore 'typing))
             (not (string-match ".*self-insert-command.*"
                                (format "%s" this-command))))
         ;; Maybe ignore minibuffer commands.
         (or (not (seq-contains-p tb-keycast-ignore 'minibuffer))
             (not (string-match (format ".+%s" this-command)
                                (format "%s" (this-command-keys))))))
    ;; TODO(irek): Handle C-u.
    ;; TODO(irek): Counter does not work for kill-line (C-k).
    (if (not (eq last-command this-command)) (setq tb-keycast--count 0))
    (setq tb-keycast--count (1+ tb-keycast--count))
    (setq tb-keycast--key (key-description (this-command-keys)))
    (setq tb-keycast--cmd (format "%s" this-command))
    ;; Set status value with last pressed key, counter and cmd name.
    ;; Left margin in (one empty spece) used only if `tab-bar-format'
    ;; is not nil and right margin is to stop status face background
    ;; color from going all the way to the right window edge.
    (setq tb-keycast--str
          (concat
           (if (not (eq tab-bar-format nil)) " ")
           (tb-keycast--format-to-string tb-keycast-format)
           " "))

    ;; Invoke `force-mode-line-update' to updates tab-bar line.
    (force-mode-line-update)))

(defun tb-keycast--format-clear ()
  "Return `tab-bar-format' without `tb-keycast--format'."
  (seq-remove (lambda (x) (eq x 'tb-keycast--format)) tab-bar-format))

(defun tb-keycast--format-wraps-p (status-width)
  "Return not nil if new status value will wrap to next line.
FRAME-WIDTH and STATUS-WIDTH should be a pixel values."
  (let* ((frame-width (frame-pixel-width))
         (format-list (tab-bar-format-list (tb-keycast--format-clear)))
         (tabs-text (mapconcat (lambda (x) (nth 2 x)) format-list))
         (tabs-width (string-pixel-width tabs-text)))
    (while (> tabs-width frame-width)
      (setq tabs-width (- tabs-width frame-width)))
    (< frame-width (+ tabs-width status-width))))

(defun tb-keycast--format ()
  "Produce keycast format string for `tab-bar-format'."
  (let* ((status-width (string-pixel-width tb-keycast--str))
         (hpos (max tb-keycast-min-width status-width)))
    (concat
     ;; Move status to next line if tabs width would cause it to wrap.
     (if (tb-keycast--format-wraps-p status-width) "\n")
     ;; Align to right if required.
     (if tb-keycast-align-right-p
         (propertize " " 'display `(space :align-to (- right (,hpos)))))
     ;; Print keycast status string.
     tb-keycast--str)))

(defun tb-keycast--start ()
  "Enable `tb-keycast-mode'."
  ;; TODO(irek): Add guard for Emacs version.
  (tab-bar-mode 1)
  (add-to-list 'tab-bar-format 'tb-keycast--format t)
  (add-hook 'pre-command-hook 'tb-keycast--update 90)
  (tb-keycast--update))

(defun tb-keycast--stop ()
  "Disable `tb-keycast-mode'."
  (remove-hook 'pre-command-hook 'tb-keycast--update)
  (setq tab-bar-format (tb-keycast--format-clear))
  (force-mode-line-update)
  ;; Hide `tab-bar-mode' if format is empty (no tabs).
  (if (eq tab-bar-format nil) (tab-bar-mode -1)))

(define-minor-mode tb-keycast-mode
  "Global minor mode that shows last pressed key in `tab-bar-mode' line.
Print corresponding function name along with key binding and
counter of how many times it was repeated."
  :global t :lighter nil
  (if tb-keycast-mode (tb-keycast--start) (tb-keycast--stop)))

(provide 'tb-keycast)

;;; tb-keycast.el ends here
