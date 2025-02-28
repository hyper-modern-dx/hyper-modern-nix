;;; hyper-modern-ai.el --- Enhanced AI Interaction for Emacs -*- lexical-binding: t -*-

;; Author: HYPER MODERN
;; URL: https://github.com/yourusername/hyper-modern-ai
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (gptel "0.1.0"))
;; Keywords: convenience, ai, tools

;;; Commentary:

;; This package provides enhanced interaction with AI language models through
;; Emacs, building on gptel. It offers multiple buffer systems for different types
;; of AI interactions:
;;
;; 1. Default Buffer - A single buffer for streamlined AI interaction ("HYPER // MODERN // AI")
;; 2. Chat Buffer - For ongoing, interactive conversations
;; 3. Response Buffer - For bulk text processing (without streaming)
;;
;; Key features:
;;
;; - Simplified interaction with a single AI buffer
;; - Dedicated chat and response buffers with different behaviors (if needed)
;; - Automatic processing of regions or whole buffers
;; - Persistent history for custom prompts with completion
;; - Easy copy/paste of AI responses
;; - Language detection based on major mode
;; - All AI buffers display in the leftmost window
;;
;; Basic usage:
;;
;; C-c a a - Interact with AI using the content of the current region or buffer (simplified single buffer)
;; C-c a c - Chat with the content of the current region or buffer
;; C-c a p - Process region/buffer with a custom prompt (with history) in the default buffer
;; C-c a y - Copy the last AI response to the kill ring
;; C-c a Y - Copy only code blocks from the last AI response
;; C-c a i - Insert the last AI response at point
;; C-c a g a - Go to the default AI buffer
;; C-c a g c - Go to the chat buffer
;; C-c a g r - Go to the response buffer
;;
;; For more details, see the README or M-x describe-function on any
;; hyper-modern-ai function.

;;; Code:

(require 'gptel)
(require 'hyper-modern-ai-utils)  ;; New utils package

;; Custom variables
(defgroup hyper-modern-ai nil
  "Enhanced AI interaction for Emacs."
  :group 'applications)

(defcustom hyper-modern-ai/chat-buffer-name "*HYPER // MODERN // AI // CHAT*"
  "Name of the buffer used for interactive AI chat sessions."
  :type 'string
  :group 'hyper-modern-ai)

(defcustom hyper-modern-ai/response-buffer-name "*HYPER // MODERN // AI // RESPONSE*"
  "Name of the buffer used for bulk AI responses."
  :type 'string
  :group 'hyper-modern-ai)

(defcustom hyper-modern-ai/default-buffer-name "*HYPER // MODERN // AI*"
  "Name of the default buffer used when only one AI interaction buffer is needed."
  :type 'string
  :group 'hyper-modern-ai)

(defcustom hyper-modern-ai/custom-prompt-history-file
  (expand-file-name "hyper-modern-ai-prompts-history.el" user-emacs-directory)
  "File to save custom prompt history."
  :type 'file
  :group 'hyper-modern-ai)

(defcustom hyper-modern-ai/max-prompt-history 30
  "Maximum number of prompts to save in history."
  :type 'integer
  :group 'hyper-modern-ai)

(defcustom hyper-modern-ai/include-buffer-info t
  "Whether to include buffer name and mode in AI requests.
When non-nil, the buffer name and major mode will be included in the request,
providing more context to the AI."
  :type 'boolean
  :group 'hyper-modern-ai)

;; Variables
(defvar hyper-modern-ai/prompt-history nil
  "History of custom prompts used.")

(defvar hyper-modern-ai/last-region-content nil
  "Last content sent from a region.")

;; Functions for buffer management
(defun hyper-modern-ai/get-or-create-chat-buffer ()
  "Get or create the AI chat buffer."
  (let ((buffer (get-buffer hyper-modern-ai/chat-buffer-name)))
    (unless buffer
      (setq buffer (gptel hyper-modern-ai/chat-buffer-name))
      (with-current-buffer buffer
        (gptel-mode)))
    buffer))

(defun hyper-modern-ai/get-or-create-response-buffer ()
  "Get or create the AI response buffer."
  (let ((buffer (get-buffer hyper-modern-ai/response-buffer-name)))
    (unless buffer
      (setq buffer (generate-new-buffer hyper-modern-ai/response-buffer-name))
      (with-current-buffer buffer
        (gptel-mode)
        ;; Disable streaming for response buffer
        (setq-local gptel-stream nil)))
    buffer))

(defun hyper-modern-ai/get-or-create-default-buffer ()
  "Get or create the default AI buffer."
  (let ((buffer (get-buffer hyper-modern-ai/default-buffer-name)))
    (unless buffer
      (setq buffer (gptel hyper-modern-ai/default-buffer-name))
      (with-current-buffer buffer
        (gptel-mode)))
    buffer))

(defun hyper-modern-ai/get-leftmost-window ()
  "Get the leftmost window in the current frame."
  (let ((windows (window-list))
        (leftmost-window nil)
        (min-x-coord most-positive-fixnum))
    (dolist (window windows)
      (let ((edges (window-edges window)))
        ;; window-edges returns (LEFT TOP RIGHT BOTTOM)
        (when (< (nth 0 edges) min-x-coord)
          (setq min-x-coord (nth 0 edges))
          (setq leftmost-window window))))
    leftmost-window))

(defun hyper-modern-ai/display-in-leftmost-window (buffer)
  "Display BUFFER in the leftmost window of the current frame."
  (let ((leftmost-window (hyper-modern-ai/get-leftmost-window)))
    (set-window-buffer leftmost-window buffer)
    (select-window leftmost-window)
    buffer))

(defun hyper-modern-ai/get-language-from-mode ()
  "Get the language name from the current major mode.
Provides a fallback for fundamental-mode."
  (let ((mode-name (symbol-name major-mode)))
    (cond
     ;; Handle fundamental-mode specially
     ((string= mode-name "fundamental-mode") "text")
     ;; Otherwise strip the -mode suffix if present
     ((string-match "\\(.*\\)-mode$" mode-name)
      (match-string 1 mode-name))
     ;; Fallback to the mode name itself
     (t mode-name))))

(defun hyper-modern-ai/format-content (content language)
  "Format CONTENT with language-specific code blocks for LANGUAGE."
  (format "```%s\n%s\n```" language content))

(defun hyper-modern-ai/get-context-info ()
  "Get context information about the current buffer."
  (when hyper-modern-ai/include-buffer-info
    (format "Source: Buffer '%s', Mode: %s\n\n"
            (buffer-name)
            (symbol-name major-mode))))

;; Region and file handling
(defun hyper-modern-ai/get-region-or-buffer-content ()
  "Get the content of the selected region or entire buffer if no region."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

;; History management
(defun hyper-modern-ai/load-prompt-history ()
  "Load prompt history from disk."
  (when (file-exists-p hyper-modern-ai/custom-prompt-history-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents hyper-modern-ai/custom-prompt-history-file)
          (let ((history (read (current-buffer))))
            (setq hyper-modern-ai/prompt-history history)))
      (error
       (message "Error loading prompt history: %s" (error-message-string err))
       (setq hyper-modern-ai/prompt-history nil)))))

(defun hyper-modern-ai/save-prompt-history ()
  "Save prompt history to disk."
  (with-temp-file hyper-modern-ai/custom-prompt-history-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 hyper-modern-ai/prompt-history (current-buffer)))))

(defun hyper-modern-ai/reload-prompt-history ()
  "Reload prompt history from disk."
  (interactive)
  (hyper-modern-ai/load-prompt-history)
  (message "Prompt history reloaded (%d entries)" (length hyper-modern-ai/prompt-history)))

(defun hyper-modern-ai/add-to-prompt-history (prompt)
  "Add PROMPT to history and save."
  (unless hyper-modern-ai/prompt-history
    (hyper-modern-ai/load-prompt-history))

  ;; Remove duplicate if exists
  (setq hyper-modern-ai/prompt-history
        (delete prompt hyper-modern-ai/prompt-history))

  ;; Add to front of list
  (push prompt hyper-modern-ai/prompt-history)

  ;; Trim to max size
  (when (> (length hyper-modern-ai/prompt-history)
           hyper-modern-ai/max-prompt-history)
    (setcdr (nthcdr (1- hyper-modern-ai/max-prompt-history)
                    hyper-modern-ai/prompt-history) nil))

  (hyper-modern-ai/save-prompt-history))

;; Main interaction commands
(defun hyper-modern-ai/chat-with-content (content &optional prompt)
  "Send CONTENT to the AI chat buffer with optional PROMPT."
  (let* ((buffer (hyper-modern-ai/get-or-create-chat-buffer))
         (language (hyper-modern-ai/get-language-from-mode))
         (context (hyper-modern-ai/get-context-info))
         (formatted-content (hyper-modern-ai/format-content content language))
         (full-prompt (if prompt
                          (concat context prompt "\n\n" formatted-content)
                        (concat context formatted-content)))
         (original-window (selected-window)))

    ;; Store content for potential later use
    (setq hyper-modern-ai/last-region-content content)

    ;; Display and set up chat buffer
    (hyper-modern-ai/display-in-leftmost-window buffer)
    (goto-char (point-max))
    (insert full-prompt)

    (condition-case err
        (gptel-send)
      (error
       (insert (format "\n\nError: %s\n\n" (error-message-string err)))
       (message "Error sending request: %s" (error-message-string err))))

    (message "HYPER // CHAT request sent")))

(defun hyper-modern-ai/bulk-process-content (content prompt)
  "Process CONTENT with PROMPT in the response buffer (no streaming)."
  (let* ((buffer (hyper-modern-ai/get-or-create-response-buffer))
         (language (hyper-modern-ai/get-language-from-mode))
         (context (hyper-modern-ai/get-context-info))
         (formatted-content (hyper-modern-ai/format-content content language))
         (full-prompt (concat context prompt "\n\n" formatted-content))
         (original-window (selected-window)))

    ;; Store content for potential later use
    (setq hyper-modern-ai/last-region-content content)

    ;; If prompt is non-empty, add to history
    (when (and prompt (not (string-empty-p prompt)))
      (hyper-modern-ai/add-to-prompt-history prompt))

    ;; Display and set up response buffer
    (hyper-modern-ai/display-in-leftmost-window buffer)
    (goto-char (point-max))
    (insert (format "--- New Request at %s ---\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert full-prompt)
    (insert "\n\n--- Response ---\n")
    (let ((response-start (point)))
      (condition-case err
          (gptel-request full-prompt
            :callback (lambda (response info)
                        (with-current-buffer buffer
                          (insert response)
                          (insert "\n\n"))))
        (error
         (insert (format "Error: %s\n\n" (error-message-string err)))
         (message "Error in gptel request: %s" (error-message-string err)))))

    (message "HYPER // RESPONSE request sent - processing...")))

;; Interactive commands
(defun hyper-modern-ai/chat-with-region-or-buffer ()
  "Start a chat with the content of the selected region or buffer."
  (interactive)
  (let ((content (hyper-modern-ai/get-region-or-buffer-content)))
    (hyper-modern-ai/chat-with-content content)))

(defun hyper-modern-ai/process-region-or-buffer ()
  "Process the selected region or buffer with a custom prompt.
Uses the default AI buffer for a streamlined experience."
  (interactive)
  (let* ((content (hyper-modern-ai/get-region-or-buffer-content))
         (prompt (completing-read "Enter prompt (or choose from history): "
                                  hyper-modern-ai/prompt-history nil nil))
         (buffer (hyper-modern-ai/get-or-create-default-buffer))
         (language (hyper-modern-ai/get-language-from-mode))
         (context (hyper-modern-ai/get-context-info))
         (formatted-content (hyper-modern-ai/format-content content language))
         (full-prompt (concat context prompt "\n\n" formatted-content)))

    ;; Store content for potential later use
    (setq hyper-modern-ai/last-region-content content)

    ;; If prompt is non-empty, add to history
    (when (and prompt (not (string-empty-p prompt)))
      (hyper-modern-ai/add-to-prompt-history prompt))

    ;; Display and set up default buffer
    (hyper-modern-ai/display-in-leftmost-window buffer)
    (goto-char (point-max))
    (insert full-prompt)

    (condition-case err
        (gptel-send)
      (error
       (insert (format "\n\nError: %s\n\n" (error-message-string err)))
       (message "Error sending request: %s" (error-message-string err))))

    (message "HYPER // MODERN // AI request with prompt sent")))

(defun hyper-modern-ai/interact-with-region-or-buffer ()
  "Interact with AI using the content of the selected region or buffer.
Uses a single AI buffer without prompting for buffer choice."
  (interactive)
  (if (use-region-p)
      (let*
          ((content
            (buffer-substring-no-properties (region-beginning) (region-end)))
           (buffer (hyper-modern-ai/get-or-create-default-buffer))
           (language (hyper-modern-ai/get-language-from-mode))
           (context (hyper-modern-ai/get-context-info))
           (formatted-content (hyper-modern-ai/format-content content language))
           (full-prompt (concat context formatted-content)))

        ;; Store content for potential later use
        (setq hyper-modern-ai/last-region-content content)

        ;; Display and set up default buffer
        (hyper-modern-ai/display-in-leftmost-window buffer)
        (goto-char (point-max))
        (insert full-prompt)

        (condition-case err
            (gptel-send)
          (error
           (insert (format "\n\nError: %s\n\n" (error-message-string err)))
           (message "Error sending request: %s" (error-message-string err))))

        (message "HYPER // MODERN // AI request sent"))

    ;; If no region is active, just go to the default AI buffer
    (hyper-modern-ai/goto-default-buffer)))


;; Extract response helper (for backward compatibility)
(defun hyper-modern-ai/extract-response (buffer &optional code-only)
  "Extract the latest response from BUFFER.
When CODE-ONLY is non-nil, only extract the last code block.
Otherwise returns the entire AI response.
Returns a cons cell (start . end) with buffer positions, or nil if no response found."
  ;; Call the utility function that has the same functionality
  (hm-utils/extract-response-region buffer code-only))

(defun hyper-modern-ai/copy-code-from-response ()
  "Copy only the last code block (content within triple backticks) from AI response."
  (interactive)
  (let* ((ai-buffer-window (hyper-modern-ai/find-ai-buffer))
         (buffer (if (window-live-p ai-buffer-window)
                     (window-buffer ai-buffer-window)
                   (or (get-buffer hyper-modern-ai/default-buffer-name)
                       (get-buffer hyper-modern-ai/chat-buffer-name)
                       (get-buffer hyper-modern-ai/response-buffer-name)))))
    (if buffer
        (with-current-buffer buffer
          (let ((code-block (hm-utils/extract-last-code-block (buffer-string))))
            (if code-block
                (progn
                  (kill-new code-block)
                  (message "Copied code block from AI response to kill ring"))
              (message "No code blocks found in the AI buffer"))))
      (message "No AI buffer found"))))


(defun hyper-modern-ai/find-ai-buffer ()
  "Find the most appropriate AI buffer to extract responses from.
Tries to find the most recently used or visible AI buffer."
  (or (get-buffer-window hyper-modern-ai/default-buffer-name)
      (get-buffer-window hyper-modern-ai/chat-buffer-name)
      (get-buffer-window hyper-modern-ai/response-buffer-name)
      (get-buffer hyper-modern-ai/default-buffer-name)
      (get-buffer hyper-modern-ai/chat-buffer-name)
      (get-buffer hyper-modern-ai/response-buffer-name)))

(defun hyper-modern-ai/copy-last-response ()
  "Copy the most recent AI response to the kill ring."
  (interactive)
  (let* ((ai-buffer-window (hyper-modern-ai/find-ai-buffer))
         (buffer (if (window-live-p ai-buffer-window)
                     (window-buffer ai-buffer-window)
                   (or (get-buffer hyper-modern-ai/default-buffer-name)
                       (get-buffer hyper-modern-ai/chat-buffer-name)
                       (get-buffer hyper-modern-ai/response-buffer-name))))
         response-region)
    (if (and buffer
             (setq response-region (hyper-modern-ai/extract-response buffer)))
        (with-current-buffer buffer
          (let ((start (car response-region))
                (end (cdr response-region)))
            (kill-new (buffer-substring-no-properties start end))
            (message "Copied AI response to kill ring")))
      (message "No AI response found"))))

(defun hyper-modern-ai/insert-last-response ()
  "Insert the most recent AI response at point."
  (interactive)
  (let* ((ai-buffer-window (hyper-modern-ai/find-ai-buffer))
         (buffer (if (window-live-p ai-buffer-window)
                     (window-buffer ai-buffer-window)
                   (or (get-buffer hyper-modern-ai/default-buffer-name)
                       (get-buffer hyper-modern-ai/chat-buffer-name)
                       (get-buffer hyper-modern-ai/response-buffer-name))))
         response-region)
    (if (and buffer
             (setq response-region (hyper-modern-ai/extract-response buffer)))
        (with-current-buffer buffer
          (let* ((start (car response-region))
                 (end (cdr response-region))
                 (response (buffer-substring-no-properties start end)))
            (with-current-buffer (window-buffer (selected-window))
              (insert response))
            (message "Inserted last response")))
      (message "No AI response found"))))

(defun hyper-modern-ai/goto-chat-buffer ()
  "Switch to the AI chat buffer."
  (interactive)
  (let ((buffer (hyper-modern-ai/get-or-create-chat-buffer)))
    (hyper-modern-ai/display-in-leftmost-window buffer)))

(defun hyper-modern-ai/goto-response-buffer ()
  "Switch to the AI response buffer."
  (interactive)
  (let ((buffer (hyper-modern-ai/get-or-create-response-buffer)))
    (hyper-modern-ai/display-in-leftmost-window buffer)))

(defun hyper-modern-ai/clear-response-buffer ()
  "Clear the contents of the response buffer."
  (interactive)
  (let ((buffer (get-buffer hyper-modern-ai/response-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (message "Response buffer cleared")))
      (message "No response buffer found"))))

(defun hyper-modern-ai/clear-default-buffer ()
  "Clear the contents of the default AI buffer."
  (interactive)
  (let ((buffer (get-buffer hyper-modern-ai/default-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (message "Default AI buffer cleared")))
      (message "No default AI buffer found"))))

(defun hyper-modern-ai/clear-chat-buffer ()
  "Clear the contents of the chat buffer."
  (interactive)
  (let ((buffer (get-buffer hyper-modern-ai/chat-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (message "Chat buffer cleared")))
      (message "No chat buffer found"))))

(defun hyper-modern-ai/manage-prompt-history ()
  "View and manage saved prompts."
  (interactive)
  (unless hyper-modern-ai/prompt-history
    (hyper-modern-ai/load-prompt-history))

  (let ((action (completing-read "Prompt History Action: "
                                 '("View" "Delete Entry" "Clear All" "Cancel")
                                 nil t)))
    (cond
     ((string= action "View")
      (with-help-window "*HYPER // MODERN // AI // PROMPTS*"
        (princ "Saved Prompts:\n\n")
        (if hyper-modern-ai/prompt-history
            (dolist (prompt hyper-modern-ai/prompt-history)
              (princ (format "• %s\n" prompt)))
          (princ "No saved prompts yet."))))

     ((string= action "Delete Entry")
      (let* ((prompt-to-delete (completing-read "Select prompt to delete: "
                                                hyper-modern-ai/prompt-history nil t)))
        (when prompt-to-delete
          (setq hyper-modern-ai/prompt-history
                (delete prompt-to-delete hyper-modern-ai/prompt-history))
          (hyper-modern-ai/save-prompt-history)
          (message "Deleted prompt: %s" prompt-to-delete))))

     ((string= action "Clear All")
      (when (yes-or-no-p "Are you sure you want to delete all saved prompts? ")
        (setq hyper-modern-ai/prompt-history nil)
        (hyper-modern-ai/save-prompt-history)
        (message "All prompts cleared")))

     (t nil))))

(defun hyper-modern-ai/goto-default-buffer ()
  "Switch to the default AI buffer."
  (interactive)
  (let ((buffer (hyper-modern-ai/get-or-create-default-buffer)))
    (hyper-modern-ai/display-in-leftmost-window buffer)))

;; Descriptive function aliases
(defalias 'hm/ai-default
  #'hyper-modern-ai/interact-with-region-or-buffer
  "Send region/buffer to default AI buffer")

(defalias 'hm/ai-chat
  #'hyper-modern-ai/chat-with-region-or-buffer
  "Send region/buffer to chat AI buffer")

(defalias 'hm/ai-prompt
  #'hyper-modern-ai/process-region-or-buffer
  "Process region/buffer with custom prompt")

(defalias 'hm/goto-default
  #'hyper-modern-ai/goto-default-buffer
  "Go to default AI buffer")

(defalias 'hm/goto-chat
  #'hyper-modern-ai/goto-chat-buffer
  "Go to chat AI buffer")

(defalias 'hm/goto-response
  #'hyper-modern-ai/goto-response-buffer
  "Go to response AI buffer")

(defalias 'hm/copy-response
  #'hyper-modern-ai/copy-last-response
  "Copy last AI response to kill ring")

(defalias 'hm/insert-response
  #'hyper-modern-ai/insert-last-response
  "Insert last AI response at point")

(defalias 'hm/prompts-manage
  #'hyper-modern-ai/manage-prompt-history
  "Manage saved prompts")

(defalias 'hm/prompts-reload
  #'hyper-modern-ai/reload-prompt-history
  "Reload prompt history from disk")

(defalias 'hm/clear-default
  #'hyper-modern-ai/clear-default-buffer
  "Clear contents of default AI buffer")

(defalias 'hm/clear-chat
  #'hyper-modern-ai/clear-chat-buffer
  "Clear contents of chat AI buffer")

(defalias 'hm/clear-response
  #'hyper-modern-ai/clear-response-buffer
  "Clear contents of response AI buffer")

(defalias 'hm/copy-code
  #'hyper-modern-ai/copy-code-from-response
  "Copy code blocks from AI response")

;; Keymap
(defvar hyper-modern-ai/keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'hm/ai-default)
    (define-key map (kbd "c") #'hm/ai-chat)
    (define-key map (kbd "p") #'hm/ai-prompt)

    ;; Go to buffers
    (define-key map (kbd "g a") #'hm/goto-default)
    (define-key map (kbd "g c") #'hm/goto-chat)
    (define-key map (kbd "g r") #'hm/goto-response)

    ;; Response management
    (define-key map (kbd "y") #'hm/copy-response)
    (define-key map (kbd "Y") #'hm/copy-code)
    (define-key map (kbd "i") #'hm/insert-response)

    ;; History and utilities
    (define-key map (kbd "h") #'hm/prompts-manage)
    (define-key map (kbd "r") #'hm/prompts-reload)

    ;; Clear buffers
    (define-key map (kbd "x a") #'hm/clear-default)
    (define-key map (kbd "x c") #'hm/clear-chat)
    (define-key map (kbd "x r") #'hm/clear-response)
    map)
  "Keymap for hyper-modern-ai commands.")

;; Initialize
(defun hyper-modern-ai/init ()
  "Initialize hyper-modern-ai."
  (unless (featurep 'gptel)
    (display-warning 'hyper-modern-ai "Package gptel is not loaded. Please install it."))
  (hyper-modern-ai/load-prompt-history))

;; Global keybinding
(global-set-key (kbd "C-c a") hyper-modern-ai/keymap)

;; Initialize on load
(hyper-modern-ai/init)

(provide 'hyper-modern-ai)
;;; hyper-modern-ai.el ends here
