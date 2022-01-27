;;; tb-keycast.el --- Tab Bar Keycast -*- lexical-binding: t -*-

;; Version: 1.6
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

(defconst tb-keycast-version "1.6"
  "Version of `tb-keycast-mode'.")

(defgroup tb-keycast nil "Tab Bar Keycast."
  :prefix "tb-keycast-" :group 'environment :version "28.1")

(defface tb-keycast-face `((t :inherit 'shadow :slant italic))
  "Face for `tb-keycast--status'."
  :group 'tb-keycast :group 'faces)

(defcustom tb-keycast-ignore '(typing  minibuffer-cmd)
  "List of ignored commands.
Possible key values: `typing', `minibuffer-cmd'."
  :group 'tb-keycast
  :type '(set (const :tag "Typing" typing)
              (const :tag "Minibuffer commands" minibuffer-cmd)))

(defcustom tb-keycast-align-right-p t
  "Align tb-keycast status to right if value is not nil."
  :group 'tb-keycast :type 'boolean)

(defcustom tb-keycast-min-width 32
  "Minimal width (in character) that is always taken by keycast status.
This helps avoid jumping of status to left and right and to next
tab-bar line when there are more tabs.  To disable use 0."
  :group 'tb-keycast :type 'number)

(defvar tb-keycast--status ""
  "Last keycast status.")

(defvar tb-keycast--counter 1
  "Key repeat counter.")

(defun tb-keycast--update ()
  "Update `tb-keycast--status' and `tb-keycast--counter' values.
Force update of mode-line and by that udate tab-bar line."
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
         (or (not (seq-contains-p tb-keycast-ignore 'minibuffer-cmd))
             (not (string-match (format ".+%s" this-command)
                                (format "%s" (this-command-keys))))))

    ;; TODO(irek): Counter does not work for kill-line (C-k).
    (setq tb-keycast--counter
          (if (eq last-command this-command)
              (1+ tb-keycast--counter) 1))

    ;; Set status value with last pressed key, counter and fun name.
    (setq tb-keycast--status
          (concat " "
                  (key-description (this-command-keys))    ;key
                  (if (> tb-keycast--counter 1)
                      (format " x%s" tb-keycast--counter)) ;counter
                  (format " (%s) " this-command)))         ;fun name

    ;; Transform status value so it looks nice.
    (setq tb-keycast--status
          (concat
           ;; Add one space as left margin if `tab-bar-format' is not
           ;; empty because we don't want status "glued" to tabs.
           (if (not (eq tab-bar-format nil)) " ")
           ;; Set min-width and apply face.
           (propertize (string-pad tb-keycast--status
                                   tb-keycast-min-width)
                       'face 'tb-keycast-face)
           ;; One space of right margin to stop face background color
           ;; from going all the way to the right window edge.
           " "))

    ;; Invoke `force-mode-line-update' to updates tab-bar line.
    (force-mode-line-update)))

(defun tb-keycast--format-clear ()
  "Return `tab-bar-format' without `tb-keycast--format'."
  (seq-remove (lambda (x) (eq x 'tb-keycast--format)) tab-bar-format))

(defun tb-keycast--format-wraps-p (frame-width status-width)
  "Return not nil if new status value will wrap to next line.
FRAME-WIDTH and STATUS-WIDTH should be a pixel values."
  (let* ((format-list (tab-bar-format-list (tb-keycast--format-clear)))
         (tabs-text (mapconcat (lambda (x) (nth 2 x)) format-list ""))
         (tabs-width (string-pixel-width tabs-text)))
    (while (> tabs-width frame-width)
      (setq tabs-width (- tabs-width frame-width)))
    (< frame-width (+ tabs-width status-width))))

(defun tb-keycast--format ()
  "Produce keycast format string for `tab-bar-format'."
  (let ((frame-width (frame-inner-width))
        (status-width (string-pixel-width tb-keycast--status)))
    (concat
     ;; Move status to next line if tabs width would cause it to wrap.
     (if (tb-keycast--format-wraps-p frame-width status-width) "\n")
     ;; Align to right if required.
     (if tb-keycast-align-right-p
         (propertize " " 'display
                     `(space :align-to (,(- frame-width status-width)))))
     ;; Print status.
     tb-keycast--status)))

(defun tb-keycast--start ()
  "Enable `tb-keycast-mode'."
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
  :global t
  :lighter nil
  (if tb-keycast-mode (tb-keycast--start) (tb-keycast--stop)))


(provide 'tb-keycast)

;;; tb-keycast.el ends here
