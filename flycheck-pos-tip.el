;;; flycheck-pos-tip.el --- Display Flycheck errors using pos-tip

;; Package-Requires: ((flycheck "0.18") (pos-tip "0.4.5"))

;;; Commentary:

;;; Code:

(require 'flycheck)
(require 'pos-tip)

;;;###autoload
(defun flycheck-pos-tip-error-messages (errors)
  "Display the tooltip that the messages of ERRORS.

Concatenate all non-nil messages of ERRORS separated by empty
lines, and display them with `pos-tip-show-no-propertize', which shows
 the messages in tooltip, depending on the number of lines."
  (-when-let (messages (-keep #'flycheck-error-message errors))
    (pos-tip-show-no-propertize
     (mapconcat 'identity messages "\n"))))

(provide 'flycheck-pos-tip)

;;; flycheck-pos-tip.el ends here
