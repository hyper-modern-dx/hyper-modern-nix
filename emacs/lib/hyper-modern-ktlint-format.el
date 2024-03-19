;;; hyper-modern-ktlint-format.el --- Automatically format a Kotlin buffer with `ktlint`.

;; Author: b7r6
;; URL: https://github.com/hyper-modern-ai/hyper-modern-ktlint-format.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'reformatter)

(defcustom hyper-modern/ktlint-format-command "ktlint"
  "The name of the `ktlint-format' command."
  :group 'ktlint-format
  :type 'string)

;;;###autoload (autoload 'hyper-modern/ktlint-format-buffer "hyper-modern-ktlint-format" nil t)
;;;###autoload (autoload 'hyper-modern/ktlint-format-on-save-mode "hyper-modern-ktlint-format" nil t)
(reformatter-define hyper-modern/ktlint-format
  :program hyper-modern/ktlint-format-command
  :args (list "--format" "--stdin" "--log-level=error")
  :group 'ktlint-format
  :lighter " ktlint")

(provide 'hyper-modern-ktlint-format)
;;; hyper-modern-ktlint-format.el ends here
