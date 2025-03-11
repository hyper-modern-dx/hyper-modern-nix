;; Standalone regex test for code blocks
;; This file demonstrates the behavior of the regex used in hyper-modern-ai without dependencies

(defun test-code-block-regex ()
  "Test the regex pattern for extracting code blocks."
  (interactive)
  
  ;; Sample text with multiple code blocks
  (let ((test-text "Here's a response with multiple code blocks:

First, let's try this Elisp:
```elisp
(defun hello-world ()
  (message \"Hello, world!\"))
```

Or maybe you'd prefer Python:
```python
def hello_world():
    print(\"Hello, world!\")
```

And here's the last code block with shell:
```shell
echo \"Hello, world!\"
```

That's all the examples."))
    
    (message "Starting regex test with sample text containing multiple code blocks...")
    
    ;; Create a temporary buffer with the test text
    (with-temp-buffer
      (insert test-text)
      
      ;; First test: Find all code blocks (forward search)
      (message "\n=== TEST 1: Find all code blocks (forward search) ===")
      (let ((all-blocks '())
            (found-count 0))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "```\\(?:[[:alnum:]]+\\)?[\n]?\\(\\(?:.\\|\n\\)*?\\)```" nil t)
            (setq found-count (1+ found-count))
            (push (match-string-no-properties 1) all-blocks)
            (message "Found block #%d: %s" found-count (match-string-no-properties 1))))
        
        (message "Total blocks found: %d" found-count)
        (message "All blocks (in reverse order): %S" all-blocks))
      
      ;; Second test: Find only the last code block (backward search)
      (message "\n=== TEST 2: Find the last code block (backward search) ===")
      (save-excursion
        (goto-char (point-max))
        (if (re-search-backward "```\\(?:[[:alnum:]]*\\)?[\n]?\\(\\(?:.\\|\n\\)*?\\)```" nil t)
            (progn
              (message "Last code block found:")
              (message "%s" (match-string-no-properties 1)))
          (message "No code block found")))
      
      ;; Explanation of the regex
      (message "\n=== REGEX EXPLANATION ===")
      (message "```\\(?:[[:alnum:]]+\\)?[\n]? - Matches the opening triple backticks and optional language name")
      (message "\\(\\(?:.\\|\n\\)*?\\)      - Captures the content between the backticks (non-greedy)")
      (message "```                      - Matches the closing triple backticks")
      (message "\nThis is the same regex used in hyper-modern-ai/extract-response"))))

;; Run the test immediately when loaded
(test-code-block-regex)

(provide 'regex-test) 