;;; hyper-modern-polyglot.el --- Description -*- lexical-binding: t -*-

;; Author: AI Assistant
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: ai, polyglot, tools
;; URL: https://example.com/hyper-modern-polyglot.el

;;; Commentary:
;; This module provides interaction with AI agents, supporting the
;; OpenAI API for assistants and tools for extensive customization.

;;; Code:

(require 'auth-source)
(require 'dash)
(require 'json)
(require 'request)
(require 'url-http)

(defgroup hyper-modern-polyglot nil
  "Customization group for the hyper-modern-polyglot Emacs module."
  :group 'external)

(defcustom hyper-modern-polyglot-openai-api-url "https://api.openai.com/v1/engines/davinci-codex/completions"
  "The URL for the OpenAI API."
  :type 'string
  :group 'hyper-modern-polyglot)

(defcustom hyper-modern-llama-cpp-api-url "http://127.0.01:8080"
  "The URL for llama.cpp's HTTP server."
  :type 'string
  :group 'hyper-modern-polyglot)

(defcustom hyper-modern-polyglot-openai-api-key nil
  "The API key for the OpenAI API."
  :type '(choice (const :tag "Not Set" nil)
                 (string :tag "API Key"))
  :group 'hyper-modern-polyglot)

(defconst hyper-modern-polyglot-log-buffer "*Hyper-Modern-Polyglot-Log*"
  "Name of the log buffer used by the hyper-modern-polyglot module.")

(defun hyper-modern/polyglot-log (message &rest args)
  "Log MESSAGE and ARGS to the hyper-modern/polyglot log buffer."
  (interactive "s")
  (let ((log-message (apply 'format message args)))
    (with-current-buffer (get-buffer-create hyper-modern-polyglot-log-buffer)
      (goto-char (point-max))
      (insert (format "%s\n" log-message)))))

(defun hyper-modern/polyglot--read-credentials ()
  "Retrieve the API key for OpenAI from either `hyper-modern/polyglot-api-key' or the auth source."
  (interactive)
  (or hyper-modern/polyglot-api-key
      (let ((creds (auth-source-search :host "api.openai.com" :max 1)))
        (when creds
          (let* ((entry (car creds))
                 (token (funcall (plist-get entry :secret))))
            token)))))

(defun hyper-modern/polyglot--send-request (api-url api-key input-text callback)
  "Send a request to the OpenAI API with INPUT-TEXT and invoke CALLBACK with the response."
  (hyper-modern/polyglot-log "Sending request to %s with input: %s" api-url input-text)
  (let ((headers (if api-key `(("Content-Type" . "application/json")
                               ("Authorization" . ,(format "Bearer %s" api-key)))
                   `(("Content-Type" . "application/json")))))
    (request
      api-url
      :type "POST"
      :headers headers
      :data (json-encode `(("prompt" . ,input-text)))
      :parser 'json-read
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (hyper-modern/polyglot-log "Request successful: %S" response)
                  (funcall callback response)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (hyper-modern/polyglot-log "Error sending request: %S" error-thrown)
                (message "Error sending request: %S" error-thrown))))))

(defun hyper-modern/polyglot-display-response (response)
  "Display the RESPONSE from the OpenAI API."
  (let ((output (assoc-default 'choices response)))
    (when output
      (let ((text (assoc-default 'text (aref output 0))))
        (with-current-buffer (get-buffer-create "*Hyper-Modern/Polyglot-Output*")
          (erase-buffer) ; Erases previous content
          (insert text)
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun hyper-modern/polyglot-ask-openai (input-text)
  "Ask the OpenAI assistant with INPUT-TEXT and display the response."
  (interactive "sOpenAI Prompt: ")
  (let ((api-key (hyper-modern/polyglot--read-credentials)))
    (if api-key
        (hyper-modern/polyglot-send-request hyper-modern/polyglot-api-url
                                            api-key
                                            input-text
                                            'hyper-modern/polyglot-display-response)
      (message "No API key available. Please set `hyper-modern/polyglot-api-key' or configure it in your auth-source."))))

(defun hyper-modern/ask-llama-cpp (input-text)
  "Ask the llama.cpp assistant with INPUT-TEXT and display the response."
  (interactive "sllama.cpp Prompt: ")

  (hyper-modern/polyglot-log (format "attempting to send request: `%S`" input-text))

  (hyper-modern/polyglot-log "testing...")
  (hyper-modern/polyglot-send-request
   hyper-modern-llama-cpp-api-url "" input-text 'hyper-modern/display-response))

;;
;; `llama.cpp` support
;;

(defconst hyper-modern/polyglot--llama-cpp-process-buffer " *llama-cpp-output*")

(defun hyper-modern/polyglot--llama-cpp-cancel ()
  "Cancel the running llama process. It will terminate TCP connection and stop server computations."
  (interactive)

  (when-let ((process (get-process llama-cpp--process)))
    (delete-process process))

  (when-let ((buffer (get-buffer hyper-modern/polyglot--llama-cpp-process-buffer)))
    (kill-buffer buffer)))

;;;###autoload
(defun hyper-modern/polyglot-llama-cpp-complete (prompt callback)
  "Complete the PROMPT using llama-cpp server. CALLBACK is called multiple times after a new token generated. It cancels the previous running llama generation if any."
  (hyper-modern/polyglot--llama-cpp-cancel)
  (let* ((buffer (get-buffer-create
                  hyper-modern/polyglot--llama-cpp-process-buffer))

         (process (make-network-process
                   :name llama-cpp--process
                   :buffer buffer
                   :host llama-cpp-host
                   :service llama-cpp-port
                   :filter (-partial #'llama-cpp--process-filter callback))))

    (process-send-string process (llama-cpp--request-body prompt))))

(provide 'hyper-modern-polyglot)

;;; hyper-modern-polyglot.el ends here
