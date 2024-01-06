;;; hyper-modern/polyglot/llama-cpp.el --- A client for llama-cpp server -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Evgeny Kurnevsky <kurnevsky@gmail.com>

;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (dash "2.19.1"))
;; Author: Evgeny Kurnevsky <kurnevsky@gmail.com>
;; Keywords: tools
;; URL: https://github.com/kurnevsky/llama.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs client for interacting with the `llama-cpp' server:
;; https://github.com/ggerganov/llama.cpp/tree/master/examples/server

;;; Code:

(require 'url-http)
(require 'dash)

(defgroup hyper-modern/polyglot--llama-cpp nil
  "Llama-cpp client."
  :group 'tools)

(defface hyper-modern/polyglot--llama-cpp-host-face
  (make-face 'special)
  "Face for hyper-modern/polyglot--llama-cpp host.")

(defface hyper-modern/polyglot--llama-cpp-port-face
  (make-face 'special)
  "Face for hyper-modern/polyglot--llama-cpp port.")

(defface hyper-modern/polyglot--llama-cpp-params-face
  (make-face 'special)
  "Face for hyper-modern/polyglot--llama-cpp params.")

(defcustom hyper-modern/polyglot--llama-cpp-host "localhost"
  "Host of the llama-cpp server."
  :type 'string
  :group 'hyper-modern/polyglot--llama-cpp)

(defface hyper-modern/polyglot--llama-cpp-n-predict-face
  (make-face 'special)
  "Face for hyper-modern/polyglot--llama-cpp n_predict.")

(defcustom hyper-modern/polyglot--llama-cpp-port 8080
  "Port of the llama-cpp server."
  :type 'natnum
  :group 'hyper-modern/polyglot--llama-cpp)

(defface hyper-modern/polyglot--llama-cpp-n-probs-face
  (make-face 'special)
  "Face for hyper-modern/polyglot--llama-cpp n_probs.")

(defcustom hyper-modern/polyglot--llama-cpp-params '(:n_predict -1 :n_probs 3)
  "Parameters for the llama-cpp /completion request."
  :type '(alist :key-type (symbol :tag "Parameter")
                :value-type (sexp :tag "Value"))
  :group 'hyper-modern/polyglot--llama-cpp)

(defvar hyper-modern/polyglot--llama-cpp--process "hyper-modern-polyglot--llama-cpp"
  "Name of the Llama CPP client process."
  :type 'string
  :group 'hyper-modern/polyglot--llama-cpp)

(defvar hyper-modern/polyglot--llama-cpp--process-buffer "*hyper-modern-polyglot--llama-cpp*"
  "Buffer for the Llama CPP client process output."
  :type 'string
  :group 'hyper-modern/polyglot--llama-cpp)

(defvar hyper-modern/polyglot--llama-cpp--rx (rx bol "data: " (group (+ nonl)) eol "\n")
  "Regex to match json data in chunked response."
  :type 'regexp
  :group 'hyper-modern/polyglot--llama-cpp)

(defvar hyper-modern/polyglot--llama-cpp--start 0
  "Index for matching regex in the buffer."
  :type 'integer
  :group 'hyper-modern/polyglot--llama-cpp)

;;;###autoload
(defun hyper-modern/polyglot--llama-cpp-complete (prompt callback)
  "Complete the PROMPT using llama-cpp server.
CALLBACK is called multiple times after a new token generated.

It cancels the previous running llama generation if any."
  (interactive-assert (and (stringp prompt) (functionp callback)))
  (hyper-modern/polyglot--llama-cpp-cancel) ; Cancel the previous process if running
  (let* ((buffer (get-buffer-create hyper-modern/polyglot--llama-cpp--process-buffer))
         (process (make-network-process
                   :name hyper-modern/polyglot--llama-cpp--process
                   :buffer buffer
                   :host hyper-modern/polyglot--llama-cpp-host
                   :service hyper-modern/polyglot--llama-cpp-port
                   :filter (-partial #'hyper-modern/polyglot--llama-cpp--process-filter callback))))
    (url-http-post-request process ; Use url-http-post-request instead of make-network-process directly
                           (hyper-modern/polyglot--llama-cpp--completion-url) ; Add the function to get completion URL
                           :content (hyper-modern/polyglot--llama-cpp--request-body prompt)))) ; Call the function to create request body

(defconst hyper-modern/polyglot--llama-cpp--completion-url ()
  "Llama-cpp completion URL."
  (format "http://%s:%d/completion" hyper-modern/polyglot--llama-cpp-host hyper-modern/polyglot--llama-cpp-port)
  :type 'string
  :group 'hyper-modern/polyglot--llama-cpp)

(defun hyper-modern/polyglot--llama-cpp--request-body (prompt)
  "Llama-cpp POST request body for the PROMPT."
  (let ((url-http-method) ; Not used, can be removed
        (url-http-proxy nil)
        (url-http-target-url (url-generic-parse-url (hyper-modern/polyglot--llama-cpp--completion-url)))
        (url-http-referer nil)
        (url-http-extra-headers `(("Content-Type" . "application/json; charset=utf-8")))
        (url-http-data
         (encode-coding-string
          (json-serialize (append '(:prompt prompt :stream t) hyper-modern/polyglot--llama-cpp-params)) 'utf-8 t)))
                                        ; url-http-create-request is not needed here, as we will send the request using url-http-post-request later.
    ))

(defun hyper-modern/polyglot--llama-cpp-cancel ()
  "Cancel the running llama process.
It will terminate TCP connection and stop server computations."
  (interactive)
  (when-let ((process (get-process hyper-modern/polyglot--llama-cpp--process)))
    (delete-process process))
  (when-let ((buffer (get-buffer hyper-modern/polyglot--llama-cpp--process-buffer)))
    (kill-buffer buffer)))

;;;###autoload
(defun hyper-modern/polyglot--llama-cpp-test ()
  "Test the llama-cpp completion function."
  (interactive)
  (unwind-protect
      (progn
        (hyper-modern/polyglot--llama-cpp-complete "Hello, world!" #'ignore))
    ;; Clean up code
    ))

(provide 'hyper-modern/polyglot--llama-cpp)
