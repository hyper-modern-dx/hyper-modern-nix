;;; swift-format.el --- Format code with swift-format -*- lexical-binding: t; -*-
;;
;; Copyright 2015 Google, Inc. All Rights Reserved.
;;
;; Package-Requires: ((emacs "24"))
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required `by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS-IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Keywords: tools, Java

;;; Commentary:

;; This package allows a user to filter code through
;; swift-format, fixing its formatting.

;; To use it, ensure the directory of this file is in your `load-path'
;; and add
;;
;;   (require 'swift-format)
;;
;; to your .emacs configuration.

;; You may also want to bind `swift-format-region' to a key:
;;
;;   (global-set-key [C-M-tab] #'swift-format-region)

;;; Code:

(defgroup swift-format nil
  "Format code using swift-format."
  :group 'tools)

(defcustom swift-format-executable
  "swift-format"
  "Location of the swift-format executable.

A string containing the name or the full path of the executable."
  :group 'swift-format
  :type '(file :must-match t :match (lambda (widget file) (file-executable-p file)))
  :risky t)

;;;###autoload
(defun swift-format-region (start end)
  "Use swift-format to format the code between START and END.
If called interactively, uses the region, if there is one.  If
there is no region, then formats the current line."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (1+ (point)))))
  (let ((cursor (point))
        (temp-buffer (generate-new-buffer " *swift-format-temp*"))
        (stderr-file (make-temp-file "swift-format")))
    (unwind-protect
        (let ((status (call-process-region
                       ;; Note that emacs character positions are 1-indexed,
                       ;; and swift-format is 0-indexed, so we have to
                       ;; subtract 1 from START to line it up correctly.
                       (point-min) (point-max)
                       swift-format-executable
                       nil (list temp-buffer stderr-file) t
		       "format"
                       ))
              (stderr
               (with-temp-buffer
                 (insert-file-contents stderr-file)
                 (when (> (point-max) (point-min))
                   (insert ": "))
                 (buffer-substring-no-properties
                  (point-min) (line-end-position)))))
          (cond
           ((stringp status)
            (error "swift-format killed by signal %s%s" status stderr))
           ((not (zerop status))
            (error "swift-format failed with code %d%s" status stderr))
           (t (message "swift-format succeeded%s" stderr)
              (delete-region (point-min) (point-max))
              (insert-buffer-substring temp-buffer)
              (goto-char cursor))))
      (delete-file stderr-file)
      (when (buffer-name temp-buffer) (kill-buffer temp-buffer)))))

(defun swift-format-buffer ()
  "Use swift-format to format the current buffer."
  (interactive)
  (swift-format-region (point-min) (point-max)))

(defalias 'swift-format 'swift-format-region)

(provide 'swift-format)

;;; swift-format.el ends here
