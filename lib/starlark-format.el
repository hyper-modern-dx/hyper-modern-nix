;;;###autoload
(defun starklark-format ()
  "Run 'buildifier' on the buffer."
  (interactive)
  (let ((bazel-format-command "buildifier")
	      (current-buffer (current-buffer))
        (oldpoint (point))
        (result-buffer (get-buffer-create "*buildifier-out*")))
    (with-current-buffer result-buffer (erase-buffer))
    (if (zerop (call-process-region (point-min) (point-max) bazel-format-command nil result-buffer nil))
        (progn
          (with-current-buffer current-buffer (replace-buffer-contents result-buffer))
          (goto-char oldpoint))
      (message "buildifier failed"))
    (kill-buffer result-buffer)))

(provide 'starlark-format)
