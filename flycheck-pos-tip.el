;;; flycheck-pos-tip.el --- Display Flycheck errors in GUI tooltips -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2014 Akiha Senda

;; Author: Akiha Senda <senda.akiha@gmail.com>
;; Maintainer: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-pos-tip
;; Keywords: tools, convenience
;; Version: 0.2-cvs
;; Package-Requires: ((dash "2.12") (flycheck "0.22") (pos-tip "0.4.6"))

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

(require 'dash)
(require 'flycheck)
(require 'pos-tip)

(defgroup flycheck-pos-tip nil
  "Display Flycheck errors in tooltips."
  :prefix "flycheck-pos-tip-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-pos-tip"))

(defcustom flycheck-pos-tip-show-function #'flycheck-pos-tip-show
  "A function to show messages in a popup.

The function shall take a single argument, a list of messages as
strings, and shall show theses messages in a graphical popup on
GUI frames (on TTY frames, this function may use any means to
report the messages)."
  :group 'flycheck-pos-tip
  :type 'function)

(defcustom flycheck-pos-tip-hide-function #'flycheck-pos-tip-hide
  "A function to hide the current popup if any.

The function shall take no arguments and shall hide the current
popup if any.  The function may be called even if there is no
popup being shown; it may not rely upon a popup being present.
The function should be a no-op in this case."
  :group 'flycheck-pos-tip
  :type 'function
  :package-version '(flycheck-pos-tip . "0.2"))

(defcustom flycheck-pos-tip-timeout 5
  "Time in seconds to hide the tooltip after.

This setting currently affects only the default
`flycheck-pos-tip-show-function'."
  :group 'flycheck-pos-tip
  :type 'number
  :package-version '(flycheck-pos-tip . "0.2"))

(defun flycheck-pos-tip-show (messages)
  "Show MESSAGES, using a pos-tip popup on GUI frames.

On TTY frames, fall back to `flycheck-display-error-messages'.
Uses `pos-tip-show' under the hood."
  (let ((message (mapconcat #'identity messages "\n\n")))
    (if (display-graphic-p)
        (pos-tip-show message nil nil nil flycheck-pos-tip-timeout)
      (display-message-or-buffer message flycheck-error-message-buffer
                                 'not-this-window))))

(defun flycheck-pos-tip-hide ()
  "Hide the Flycheck tooltip."
  (pos-tip-hide))

;;;###autoload
(defun flycheck-pos-tip-error-messages (errors)
  "Display ERRORS, using a graphical tooltip on GUI frames."
  (when errors
    (-when-let (messages (-keep #'flycheck-error-format-message-and-id errors))
      (funcall flycheck-pos-tip-show-function messages))))

(defun flycheck-pos-tip-hide-messages ()
  "Hide messages currently being shown if any."
  (funcall flycheck-pos-tip-hide-function))

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
GUI tooltip.  Falls back to `flycheck-pos-tip-tty-show-function'
on TTY frames.  You can provide your own functions to show and
hide the popup via `flycheck-pos-tip-show-function' and
`flycheck-pos-tip-hide-function' respectively.  For instance you
may use the popular popup.el library (see URL
`https://github.com/auto-complete/popup-el')."
  :global t
  :group 'flycheck
  (let ((hooks '(post-command-hook focus-out-hook)))
    (if flycheck-pos-tip-mode
        (progn
          (setq flycheck-pos-tip-old-display-function
                flycheck-display-errors-function
                flycheck-display-errors-function
                #'flycheck-pos-tip-error-messages)
          (dolist (hook hooks)
            (add-hook hook #'flycheck-pos-tip-hide-messages)))
      (setq flycheck-display-errors-function
            flycheck-pos-tip-old-display-function)
      (dolist (hook hooks)
        (remove-hook hook 'flycheck-pos-tip-hide-messages)))))

(provide 'flycheck-pos-tip)

;;; flycheck-pos-tip.el ends here
