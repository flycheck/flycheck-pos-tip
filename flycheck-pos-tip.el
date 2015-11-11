;;; flycheck-pos-tip.el --- Flycheck errors display in tooltip

;; Copyright (C) 2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2014 Akiha Senda

;; Author: Akiha Senda <senda.akiha@gmail.com>
;; Maintainer: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-pos-tip
;; Keywords: tools, convenience
;; Version: 0.0.1
;; Package-Requires: ((dash "2.12") (flycheck "0.18") (pos-tip "0.4.6"))

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
;;   (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

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
strings, and shall show theses messages in a graphical popup."
  :group 'flycheck-pos-tip
  :type 'function)

(defun flycheck-pos-tip-show (messages)
  "Show a pos-tip popup with MESSAGES.

Uses `pos-tip-show' under the hood."
  (pos-tip-show (mapconcat #'identity messages "\n\n")))

(defun flycheck-pos-tip-hide ()
  "Hide the Flycheck tooltip."
  (pos-tip-hide))

;;;###autoload
(defun flycheck-pos-tip-error-messages (errors)
  "Display ERRORS in a graphical tooltip."
  (when errors
    (-when-let (messages (-keep #'flycheck-error-format-message-and-id errors))
      (funcall flycheck-pos-tip-show-function messages))))

(defvar flycheck-pos-tip-old-display-function nil
  "The former value of `flycheck-display-errors-function'.")

;;;###autoload
(define-minor-mode flycheck-pos-tip-mode
  "A minor mode to show Flycheck error messages in a popup."
  :global t
  :group 'flycheck
  (if flycheck-pos-tip-mode
      (progn
        (setq flycheck-pos-tip-old-display-function
              flycheck-display-errors-function
              flycheck-display-errors-function
              #'flycheck-pos-tip-error-messages)
        (add-hook 'post-command-hook #'flycheck-pos-tip-hide))
    (setq flycheck-display-errors-function
          flycheck-pos-tip-old-display-function)
    (remove-hook 'post-command-hook 'flycheck-pos-tip-hide)))

(provide 'flycheck-pos-tip)

;;; flycheck-pos-tip.el ends here
