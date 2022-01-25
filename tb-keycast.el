;;; tb-keycast.el --- Tab Bar Keycast -*- lexical-binding: t -*-

;; Version: 1.3
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

(defgroup tb-keycast nil
  "Tab Bar Keycast."
  :prefix "tb-keycast-"
  :group 'environment
  :version "28.1")

;;;; Faces:

(defface tb-keycast-key-face
  '((t :inherit shadow :weight bold))
  "Face of pressed key."
  :group 'tb-keycast
  :group 'faces)

(defface tb-keycast-fun-name-face
  '((t :inherit shadow :slant italic))
  "Face of pressed key corresponding function name."
  :group 'tb-keycast
  :group 'faces)

(defface tb-keycast-repeat-number-face
  '((t :inherit shadow))
  "Face of number that shows repeat count."
  :group 'tb-keycast
  :group 'faces)

;;;; Vars:

(defcustom tb-keycast-status-align 'right
  "Align of tb-keycast status withing tab-bar line.
Value is a symbol either `left' or `right'."
  :group 'tb-keycast
  :type '(choice (const :tag "left" left)
                 (const :tag "right" right)))

(defcustom tb-keycast-status-min-width 32
  "Minimal width (in character) that is taken by keycast status.

If status is longer than provided value it will take more space
anyway but having minimal width is useful to avoid jumping of
status to left and right (when status is aligned to the right)
and jumping to next tab-bar line when there are more tabs pushing
status close to the right edge.

Set to 0 to disable."
  :group 'tb-keycast
  :type 'number)

(defconst tb-keycast-version "1.3"
  "Version string of `tb-keycast-mode'.")

(defvar tb-keycast--status ""
  "Last used binding with corresponding command.")

(defvar tb-keycast--repeat 1
  "How many times last binding was used in a row.")

;;;; Functions:

(defun tb-keycast--update ()
  "Update `tb-keycast--status' and `tb-keycast--repeat' values.
Force update of mode-line and by that udate tab-bar line."
  (when (and
         ;; Ignore undefined bindings.
         this-command

         ;; Ignore regular typing.
         (not (string-match ".*self-insert-command.*"
                            (format "%s" this-command)))

         ;; Ignore mouse drag.
         (not (string-match "^#"
                            (format "%s" this-command)))
         
         ;; Ignore minibuffer commands.
         (not (string-match (format ".+%s" this-command)
                            (format "%s" (this-command-keys)))))

    ;; TODO Repeat does not work for kill-line (C-k)
    (setq tb-keycast--repeat
          (if (eq last-command this-command)
              (1+ tb-keycast--repeat) 1))

    (setq tb-keycast--status
          (format " %s%s %s "
                  (propertize (key-description (this-command-keys))
                              'face 'tb-keycast-key-face)
                  (propertize (if (> tb-keycast--repeat 1)
                                  (format " x%s" tb-keycast--repeat)
                                "")
                              'face 'tb-keycast-repeat-number-face)
                  (propertize (format "%s" this-command)
                              'face 'tb-keycast-fun-name-face)))

    ;; Set min width to avoid constant jumps to right and left.
    (setq tb-keycast--status
          (string-pad tb-keycast--status tb-keycast-status-min-width))
    
    ;; `force-mode-line-update' also updates tab-bar line.
    (force-mode-line-update)))

(defun tb-keycast--format-clear ()
  "Return `tab-bar-format' without `tb-keycast--format'."
  (seq-remove (lambda (x) (eq x 'tb-keycast--format)) tab-bar-format))

(defun tb-keycast--format-wraps-p ()
  "Return not nil if new status value will wrap to next line."
  (let* ((window-width (window-pixel-width))
	 (format-list (tab-bar-format-list (tb-keycast--format-clear)))
	 (tabs-text (mapconcat (lambda (x) (nth 2 x)) format-list ""))
	 (tabs-width (string-pixel-width tabs-text))
	 (status-width (string-pixel-width tb-keycast--status)))

    (while (> tabs-width window-width)
      (setq tabs-width (- tabs-width window-width)))

    (< window-width (+ tabs-width status-width))))

(defun tb-keycast--format ()
  "Keycast format for `tab-bar-format' variable."
  (concat
   ;; Move status to next line if tabs width would cause it to wrap.
   (if (tb-keycast--format-wraps-p) "\n")

   ;; Align to right if required
   (if (eq tb-keycast-status-align 'right)
       (let* ((window-width (window-pixel-width))
              (status-width (string-pixel-width tb-keycast--status))
              (hpos-px (- (window-pixel-width)
                          (string-pixel-width tb-keycast--status))))
         (propertize " " 'display `(space :align-to (,hpos-px)))))

   ;; Print status.
   tb-keycast--status))

(defun tb-keycast--start ()
  "Enable keycast."
  (tab-bar-mode 1)
  (add-to-list 'tab-bar-format 'tb-keycast--format t)
  (add-hook 'pre-command-hook 'tb-keycast--update 90))

(defun tb-keycast--stop ()
  "Disable keycast."
  (remove-hook 'pre-command-hook 'tb-keycast--update)
  (setq tab-bar-format (tb-keycast--format-clear))
  (force-mode-line-update))

(define-minor-mode tb-keycast-mode
  "Global minor mode that shows last pressed key in `tab-bar-mode' line.
Print corresponding function name along with key binding and
number of how many times it was repeated."
  :global t
  :lighter nil
  (if tb-keycast-mode (tb-keycast--start) (tb-keycast--stop)))


(provide 'tb-keycast)

;;; tb-keycast.el ends here
