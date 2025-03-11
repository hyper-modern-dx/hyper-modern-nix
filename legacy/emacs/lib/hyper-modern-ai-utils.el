;; hyper-modern-ai-utils.el --- Utility functions for hyper-modern-ai -*- lexical-binding: t -*-

;; Author: HYPER MODERN
;; URL: https://github.com/yourusername/hyper-modern-ai
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, ai, tools

;;; Commentary:

;; This file contains utility functions used by hyper-modern-ai.
;; These are extracted from the main package to allow for easier testing
;; and modular development.

;;; Code:

(defun hm-utils/find-code-blocks (text &optional last-only)
  "Find code blocks within TEXT.
When LAST-ONLY is non-nil, only return the last code block.
Returns a list of found code blocks, or nil if none found."
  (with-temp-buffer
    (insert text)
    (let ((blocks '()))
      (if last-only
          ;; Just find the last block with backward search
          (progn
            (goto-char (point-max))
            (when (re-search-backward "```\\(?:[[:alnum:]]*\\)?[\n]?\\(\\(?:.\\|\n\\)*?\\)```" nil t)
              (setq blocks (list (string-trim-right (match-string-no-properties 1))))))
        ;; Find all blocks with forward search
        (progn
          (goto-char (point-min))
          (while (re-search-forward "```\\(?:[[:alnum:]]+\\)?[\n]?\\(\\(?:.\\|\n\\)*?\\)```" nil t)
            (push (string-trim-right (match-string-no-properties 1)) blocks))
          ;; Reverse to get in original order
          (setq blocks (nreverse blocks))))
      blocks)))

(defun hm-utils/extract-last-code-block (text)
  "Extract only the last code block from TEXT.
Returns the content of the code block or nil if none found."
  (let ((blocks (hm-utils/find-code-blocks text t)))
    (car blocks)))

(defun hm-utils/extract-all-code-blocks (text)
  "Extract all code blocks from TEXT.
Returns a list of code blocks or nil if none found."
  (hm-utils/find-code-blocks text))

(defun hm-utils/concatenate-code-blocks (blocks &optional separator)
  "Concatenate code BLOCKS with an optional SEPARATOR.
If SEPARATOR is nil, uses a double newline."
  (let ((sep (or separator "\n\n")))
    (mapconcat #'identity blocks sep)))

(defun hm-utils/get-from-buffer-positions (buffer start end)
  "Get text from BUFFER between START and END positions."
  (with-current-buffer buffer
    (buffer-substring-no-properties start end)))

(defun hm-utils/extract-response-region (buffer &optional code-only)
  "Extract the latest response region from BUFFER.
When CODE-ONLY is non-nil, only extract the last code block.
Returns a cons cell (start . end) with buffer positions, or nil if no response found."
  (with-current-buffer buffer
    (save-excursion
      (if code-only
          ;; Extract only the last code block
          (progn
            (goto-char (point-max))
            (if (re-search-backward "```\\(?:[[:alnum:]]*\\)?[\n]?\\(\\(?:.\\|\n\\)*?\\)```" nil t)
                (cons (match-beginning 1) (match-end 1))
              nil))  ;; Return nil if no code block found
        ;; For regular response, return the whole buffer content
        (cons (point-min) (point-max))))))

(provide 'hyper-modern-ai-utils)
;;; hyper-modern-ai-utils.el ends here 