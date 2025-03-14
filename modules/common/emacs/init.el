;;; init.el --- -*- lexical-binding: t -*-

;;
;; "On The Design Of Text Editors" - https://arxiv.org/abs/2008.06030
;;

;;
;;
;; “There is always a point at which the terrorist ceases to manipulate the media
;;  gestalt. A point at which the violence may well escalate, but beyond which the
;;  terrorist has become symptomatic of the media gestalt itself. Terrorism as we
;;  ordinarily understand it is inately media-related. The Panther Moderns differ from
;;  other terrorists precisely in their degree of self-consciousness, in their
;;  awareness of the extent to which media divorce the act of terrorism from the
;;  original sociopolitical intent.”
;;
;; “Skip it.” Case said.
;;

;; ============================================================
;; memory // performance // optimization
;; ============================================================

(defvar hyper-modern/gc-cons-threshold (* 256 1024 1024))

(setq 
 gc-cons-threshold hyper-modern/gc-cons-threshold
 gc-cons-percentage 0.1)

(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (setq gc-cons-threshold most-positive-fixnum)))

(add-hook
 'minibuffer-exit-hook
 (lambda ()
   (garbage-collect)
   (setq gc-cons-threshold hyper-modern/gc-cons-threshold)))

(setq copy-region-blink-delay 0)

;; ============================================================
;; ui // reinit
;; ============================================================

(setq inhibit-startup-screen t)

(blink-cursor-mode       +1)
(column-number-mode       1)
(column-number-mode      +1)
(display-time-mode       +1)
(flymake-mode            -1)
(global-auto-revert-mode +1)
(global-hl-line-mode      1)
(global-hl-line-mode     +1)
(global-whitespace-mode  -1)
(menu-bar-mode           -1)
(menu-bar-mode           -1)
(show-paren-mode         +1)

(put 'upcase-region 'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)


(setq-default indent-tabs-mode nil)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; TODO[b7r6]: decide what to do about this...
;; (xterm-mouse-mode 1)

(when (boundp 'standard-display-table)
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))

  (set-display-table-slot standard-display-table 'vertical-border ?│))

(setq-default visible-bell nil)
(setq-default ring-bell-function #'ignore)

(defun remove-bold-italic-from-all-faces ()
  "Remove all bold and italic attributes from all faces."
  (mapc (lambda (face)
          (when (face-attribute face :weight nil t)
            (set-face-attribute face nil :weight 'normal))
          (when (face-attribute face :slant nil t)
            (set-face-attribute face nil :slant 'normal)))
        (face-list)))

(remove-bold-italic-from-all-faces)

;; ============================================================
;; hyper // modern // libs
;; ============================================================

(require 'base16-theme)

(add-to-list 'load-path "~/.emacs.d/lib/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(load-library "~/.emacs.d/lib/smart-split.el")
(load-library "~/.emacs.d/themes/base16-ono-sendai.el")

(add-hook 
 'after-init-hook
 (lambda ()
   ;; (load-theme 'base16-ono-sendai t)
   (smart-split)))

(condition-case nil
    (progn
      (require 'nerd-icons)
      (setq doom-modeline-icon t)
      (setq doom-modeline-major-mode-icon t))

  (error
   (message "Nerd icons not available, disabling icons in modeline")
   (setq doom-modeline-icon nil)))

;; ============================================================
;; hyper // modern // ai
;; ============================================================

;; TODO[b7r6]: !! get keys in `sops.nix` / vault !!
(when (require 'gptel nil t)
  (setq gptel-model "gpt-4o")
  (setq gptel-max-tokens 4096))

;; ============================================================
;; completion // minibuffer // read
;; ============================================================

(require 'vertico)
(require 'orderless)
(require 'marginalia)
(require 'which-key)
(require 'rainbow-delimiters)
(require 'smartparens)

(vertico-mode 1)
(marginalia-mode 1)
(which-key-mode 1)
(smartparens-global-mode 1)

(setq 
 completion-styles '(orderless basic)
 completion-category-defaults nil
 completion-category-overrides '((file (styles partial-completion))))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(global-set-key (kbd "C-x C-r") 'consult-recent-file)

;; ============================================================
;; directories // projects // ripgrep
;; ============================================================

(require 'projectile)

(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'rg)

(setq rg-group-result t)
(setq rg-context-line-count 2)
(setq rg-show-columns t)
(setq rg-command-line-flags '("--type=all"))

(require 'wgrep)

(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

(when (require 'dirvish nil t)
  (dirvish-override-dired-mode))

;; ============================================================
;; windew // frame // movement
;; ============================================================

(defun hyper-modern/scratch ()
  (let ((dir (if buffer-file-name
	         (file-name-directory buffer-file-name)
               default-directory)))

    (get-buffer-create (concat dir "elisp-scratch.el"))))

(defun hyper-modern/other ()
  (let ((buf (other-buffer (current-buffer))))
    (if (or (null buf) (eq buf (current-buffer)))
        (hyper-modern/scratch)
      buf)))

(defun hyper-modern/switch ()
  (let ((nw (next-window))
        (cb (current-buffer)))
    (with-selected-window nw
      (when (eq (window-buffer) cb)
        (switch-to-buffer (hyper-modern/other))))))

(defun hyper-modern/hsplit (&optional size)
  (interactive)
  (split-window-right size)
  (hyper-modern/switch))

(defun hyper-modern/vsplit (&optional size)
  (interactive)
  (split-window-below size)
  (hyper-modern/switch))

(defun hyper-modern/rotate-windows ()
  (interactive)
  (let* ((original-buffer (current-buffer))
         (windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (num-windows (length windows)))

    (when (> num-windows 1)
      (dotimes (i num-windows)
        (set-window-buffer
         (nth i windows) (nth (mod (+ i 1) num-windows) buffers)))

      (let ((original-window (get-buffer-window original-buffer t)))
        (when original-window
          (select-window original-window))))))

;; ============================================================
;; mode // line
;; ============================================================

(require 'doom-modeline)
(doom-modeline-mode 1)

;; ============================================================
;; mode // hacking
;; ============================================================

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

(require 'paredit-everywhere)
(add-hook 'prog-mode-hook #'paredit-everywhere-mode)

(require 'direnv)
(direnv-mode 1)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ============================================================
;; format // treesit // apheleia
;; ============================================================

(require 'apheleia)
(apheleia-global-mode +1)

(require 'treesit-auto)
(global-treesit-auto-mode)

(require 'treesit)
(add-to-list 'major-mode-remap-alist '(python-mode . python-mode))

;; ============================================================
;; nix // mode
;; ============================================================

(require 'nix-mode)

(with-eval-after-load 'nix-mode
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

;; ============================================================
;; python // mode
;; ============================================================

(require 'python)


(with-eval-after-load 'python
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

;; ============================================================
;; typescript // mode
;; ============================================================

(require 'typescript-ts-mode)

(with-eval-after-load 'typescript-ts-mode
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-ts-mode))

  (setq typescript-indent-level 2)
  (setq js-indent-level 2))

;; ============================================================
;; cc // mode
;; ============================================================

(require 'cc-mode)

(with-eval-after-load 'cc-mode
  )

;; ============================================================
;; java // mode
;; ============================================================

(when (fboundp 'java-ts-mode)
  (with-eval-after-load 'java-ts-mode
    (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))))

(when (fboundp 'json-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (with-eval-after-load 'json-ts-mode
    ))

(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(with-eval-after-load 'yaml-mode
  )

(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(with-eval-after-load 'markdown-mode
  )

(require 'lua-mode)

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(require 'haskell-mode)

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

(with-eval-after-load 'rust-mode
  )

(require 'swift-mode)
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))
(with-eval-after-load 'swift-mode
  )

(require 'fsharp-mode)

(with-eval-after-load 'fsharp-mode
  (add-to-list 'auto-mode-alist '("\\.fs[iylx]?\\'" . fsharp-mode)))

(require 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'zig-mode)
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(require 'just-mode)
(add-to-list 'auto-mode-alist '("Justfile\\'" . just-mode))
(add-to-list 'auto-mode-alist '("\\.just\\'" . just-mode))

(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(when (require 'prisma-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.prisma\\'" . prisma-mode)))

(require 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

;; ============================================================
;; Eglot (LSP) Configuration
;; ============================================================
(require 'eglot)

;; Configure server programs
(add-to-list 'eglot-server-programs
             '((typescript-ts-mode tsx-ts-mode js-ts-mode jsx-ts-mode)
               . ("typescript-language-server" "--stdio")))

(add-to-list 'eglot-server-programs
             '(python-mode . ("pyright-langserver" "--stdio")))

(add-to-list 'eglot-server-programs
             '((c-mode c++-mode) . ("clangd")))

(add-to-list 'eglot-server-programs
             '(rust-mode . ("rust-analyzer")))

(add-to-list 'eglot-server-programs
             '(java-ts-mode . ("java-language-server")))

(add-to-list 'eglot-server-programs
             '(nix-mode . ("nixd")))

(setq eglot-stay-out-of '("flymake"))

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'jsx-ts-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'nix-mode-hook 'eglot-ensure)

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (eglot-inlay-hints-mode -1)
            (flymake-mode -1)))

;; ============================================================
;; company // completion
;; ============================================================

(require 'company)

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)

(global-company-mode 1)
(global-set-key (kbd "M-TAB") #'company-complete)

;; ============================================================
;; Magit Configuration
;; ============================================================

(require 'magit)

(defun my-magit-display-buffer-function (buffer)
  "Display BUFFER in the rightmost window without splitting."
  (let ((window (if (one-window-p)
	            (selected-window)
	          (window-at (- (frame-width) 2) 1))))

    (select-window window)
    (set-window-buffer window buffer)
    window))

(setq 
 magit-display-buffer-function 
 #'my-magit-display-buffer-function)

;; ============================================================
;; edit // compile // test
;; ============================================================

(require 'compile)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; ============================================================
;; v // term
;; ============================================================

(defun setup-vterm ()
  (when (fboundp 'vterm-mode)
    (setq vterm-keymap-exceptions '("M-/" "M-N" "M-P" "M-i" "M-z"))
    (with-eval-after-load 'vterm
      (define-key vterm-mode-map (kbd "M-N") 'windmove-right)
      (define-key vterm-mode-map (kbd "M-P") 'windmove-left))))

(add-hook 
 'vterm-mode-hook 
 (lambda ()
   (setq-local global-hl-line-mode nil)
   (setup-vterm)))

;; ============================================================
;; tmux // copy // paste
;; ============================================================

;; (unless window-system
;;   (require 'xterm-clipboard-mode nil t)
;;   (xterm-clipboard-mode 1))

;; (setq select-enable-clipboard t)
;; (setq select-enable-primary t)
;; (setq save-interprogram-paste-before-kill t)

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(defun sync-kill-to-tmux-clipboard (str &rest _)
  "Sync the text added to the kill ring to tmux clipboard."
  (when (and str (getenv "TMUX") (executable-find "tmux"))
    (let ((process-connection-type nil)
          (proc (start-process "tmux-clipboard" nil "tmux" "set-buffer" str)))

      ;; Process sentinel to handle any errors
      (set-process-sentinel
       proc
       (lambda (proc _)
         (when (not (= (process-exit-status proc) 0))
           (message "Warning: Failed to sync to tmux clipboard")))))))

(advice-add 'kill-new :after #'sync-kill-to-tmux-clipboard)

;; ============================================================
;; dashboard // mode
;; ============================================================

(when (require 'dashboard nil t)
  (defvar my-custom-banner-file (make-temp-file "emacs-dashboard-banner-" nil ".txt"))
  (defvar my-custom-banner-text "HYPER // MODERN // NIX ")

  (with-temp-file my-custom-banner-file
    (insert my-custom-banner-text))

  (setq dashboard-startup-banner my-custom-banner-file)
  
  (setq dashboard-banner-logo-title
        "it was the style that mattered and the style was the same.
the moderns were mercenaries, practical jokers, nihilistic tehcnofetishists.")

  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((projects . 5)
                          (recents . 5)))

  (dashboard-setup-startup-hook))

;; ============================================================
;; general // key // bind
;; ============================================================

(require 'general)

(general-define-key
 "C-c q"   'join-line
 "C-c r"   'revert-buffer
 "C-j"     'newline-and-indent

 "C-x C-+" 'text-scale-increase
 "C-x C--" 'text-scale-decrease
 "C-x C-r" 'rg-dwim-project-dir
 "C-x d"   'consult-recent-file

 "M-/"     'undo
 "M-N"     'windmove-right
 "M-P"     'windmove-left
 "M-z"     'apheleia-format-buffer

 "C-M-r"   'consult-ripgrep
 "M-R"     'hyper-modern/rotate-windows
 "C-x 2"   'hyper-modern/vsplit
 "C-x 3"   'hyper-modern/hsplit
 "C-x g"   'magit-status
 "C-s"     'consult-line
 "C-c r"   'consult-ripgrep)
