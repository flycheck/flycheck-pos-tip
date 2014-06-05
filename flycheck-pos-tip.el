;;; flycheck-pos-tip.el --- Flycheck errors display in tooltip

;; Copyright (C) 2014  Akiha Senda

;; Author: Akiha Senda <senda.akiha@gmail.com>
;; URL: https://github.com/flycheck/flycheck-pos-tip
;; Keywords: tools, convenience
;; Version: 0.0.1
;; Package-Requires: ((flycheck "0.18") (popup "0.5.0"))

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

;; Add error message display method for Flycheck.

;;;; Setup

;; (eval-after-load 'flycheck
;;   '(custom-set-variables
;;    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;;; Code:

(require 'flycheck)
(require 'popup)

(defgroup flycheck-pos-tip nil
  "Flycheck errors display in tooltip"
  :prefix "flycheck-pos-tip-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-pos-tip"))

;;;###autoload
(defun flycheck-pos-tip-error-messages (errors)
  "Display the tooltip that the messages of ERRORS.

Concatenate all non-nil messages of ERRORS separated by empty
lines, and display them with `pos-tip-show-no-propertize', which shows
 the messages in tooltip, depending on the number of lines."
  (-when-let (messages (-keep #'flycheck-error-message errors))
    (popup-tip
     (mapconcat 'identity messages "\n"))))

(provide 'flycheck-pos-tip)

;;; flycheck-pos-tip.el ends here
