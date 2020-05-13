;;; flycheck-pos-tip.el --- Display Flycheck errors in GUI tooltips -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2014 Akiha Senda

;; Author: Akiha Senda <senda.akiha@gmail.com>
;;     Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-pos-tip
;; Keywords: tools, convenience
;; Version: 0.4-cvs
;; Package-Requires: ((emacs "24.1") (flycheck "0.22") (pos-tip "0.4.6"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; Provide an error display function to show errors in a tooltip.

;;;; Setup

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;;; Code:

(require 'flycheck)
(require 'pos-tip)

(defgroup flycheck-pos-tip nil
  "Display Flycheck errors in tooltips."
  :prefix "flycheck-pos-tip-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-pos-tip"))

(defcustom flycheck-pos-tip-max-width nil
  "If non-nil, the max width of the tooltip in chars."
  :group 'flycheck-pos-tip
  :type '(choice (const :tag "Auto" nil)
                 (integer :tag "Characters"))
  :package-version '(flycheck-pos-tip . "0.4"))

(defcustom flycheck-pos-tip-timeout 5
  "Time in seconds to hide the tooltip after."
  :group 'flycheck-pos-tip
  :type 'number
  :package-version '(flycheck-pos-tip . "0.2"))

(defcustom flycheck-pos-tip-display-errors-tty-function
  #'flycheck-display-error-messages
  "Fallback function for error display on TTY frames.

Like `flycheck-display-errors-function'; called to show error
messages on TTY frames if `flycheck-pos-tip-mode' is active."
  :group 'flycheck-pos-tip
  :type 'function
  :package-version '(flycheck-pos-tip . "0.2"))

(defvar-local flycheck-pos-tip--last-pos nil
  "Last position for which a pos-tip was displayed.")

(defun flycheck-pos-tip--check-pos ()
  "Update flycheck-pos-tip--last-pos, returning t if there was no change."
  (equal flycheck-pos-tip--last-pos
         (setq flycheck-pos-tip--last-pos
               (list (current-buffer) (buffer-modified-tick) (point)))))

(defun flycheck-pos-tip-error-messages (errors)
  "Display ERRORS, using a graphical tooltip on GUI frames."
  (when errors
    (if (display-graphic-p)
        (let ((message (flycheck-help-echo-all-error-messages errors))
              (line-height (car (window-line-height))))
          (flycheck-pos-tip--check-pos)
          (pos-tip-show message nil nil nil flycheck-pos-tip-timeout
                        flycheck-pos-tip-max-width nil
                        ;; Add a little offset to the tooltip to move it away
                        ;; from the corresponding text in the buffer.  We
                        ;; explicitly take the line height into account because
                        ;; pos-tip computes the offset from the top of the line
                        ;; apparently.
                        nil (and line-height (+ line-height 5))))
      (funcall flycheck-pos-tip-display-errors-tty-function errors))))

(defun flycheck-pos-tip-hide-messages ()
  "Hide messages currently being shown if any."
  (unless (flycheck-pos-tip--check-pos)
    (if (display-graphic-p)
        (pos-tip-hide)
      (flycheck-hide-error-buffer))))

(defvar flycheck-pos-tip-old-display-function nil
  "The former value of `flycheck-display-errors-function'.")

;;;###autoload
(define-minor-mode flycheck-pos-tip-mode
  "A minor mode to show Flycheck error messages in a popup.

When called interactively, toggle `flycheck-pos-tip-mode'.  With
prefix ARG, enable `flycheck-pos-tip-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `flycheck-pos-tip-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`flycheck-pos-tip-mode'.  Otherwise behave as if called
interactively.

In `flycheck-pos-tip-mode' show Flycheck's error messages in a
GUI tooltip.  Falls back to `flycheck-display-error-messages' on
TTY frames."
  :global t
  :group 'flycheck
  (let ((hooks '(post-command-hook focus-out-hook)))
    (cond
     ;; Use our display function and remember the old one but only if we haven't
     ;; yet configured it, to avoid activating twice.
     ((and flycheck-pos-tip-mode
           (not (eq flycheck-display-errors-function
                    #'flycheck-pos-tip-error-messages)))
      (setq flycheck-pos-tip-old-display-function
            flycheck-display-errors-function
            flycheck-display-errors-function
            #'flycheck-pos-tip-error-messages)
      (dolist (hook hooks)
        (add-hook hook #'flycheck-pos-tip-hide-messages)))
     ;; Reset the display function and remove ourselves from all hooks but only
     ;; if the mode is still active.
     ((and (not flycheck-pos-tip-mode)
           (eq flycheck-display-errors-function
               #'flycheck-pos-tip-error-messages))
      (setq flycheck-display-errors-function
            flycheck-pos-tip-old-display-function
            flycheck-pos-tip-old-display-function nil)
      (dolist (hook hooks)
        (remove-hook hook 'flycheck-pos-tip-hide-messages))))))

(provide 'flycheck-pos-tip)

;;; flycheck-pos-tip.el ends here
