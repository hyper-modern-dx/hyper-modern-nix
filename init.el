;;
;; prelude (inspired by @ianyepan)
;;

;; `emacs` memory management tuning

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil  ; Corrected variable name
      site-run-file nil)

(defvar b7r6/gc-cons-threshold (* 256 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold b7r6/gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (garbage-collect)
            (setq gc-cons-threshold b7r6/gc-cons-threshold)))

;; bootstrap `straight.el`
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;;
;; global `emacs` settings
;;

(use-package emacs
  :ensure nil

  :preface
  (defvar b7r6/indent-width 2)
  (defvar b7r6/max-columns 80)

  :config
  (setq user-full-name "b7r6")

  (setq-default default-directory "~/src")
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width b7r6/indent-width)

  (setq auto-save-default nil)
  (setq confirm-kill-processes nil)
  (setq debug-on-error nil)
  (setq echo-keystrokes 0.1)
  (setq indent-tabs-mode nil)
  (setq inhibit-startup-message t)
  (setq initial-scratch-message "")
  (setq make-backup-files nil)
  (setq pop-up-windows nil)
  (setq require-final-newline t)
  (setq resize-mini-windows nil)
  (setq ring-bell-function 'ignore)  ; Removed duplicate
  (setq scroll-conservatively 10000)
  (setq scroll-step 1)
  (setq select-enable-clipboard t)  ; Removed duplicate
  (setq split-height-threshold nil)
  (setq split-width-threshold nil)
  (setq transient-mark-mode t)

  ;; global built-in modes
  (blink-cursor-mode t)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (global-whitespace-mode -1)
  (menu-bar-mode -1)
  (show-paren-mode 1)

  ;; no pipes in vertical border
  (set-display-table-slot
   standard-display-table 'vertical-border ?│)

  ;; no italics ever
  (set-face-italic 'font-lock-comment-face nil)
  (set-face-italic 'font-lock-comment-delimiter-face nil)

  ;; (set-face-foreground
  ;;  'vertical-border
  ;;  ;; (face-background 'vertical-border nil t)
  ;;  ;; (face-foreground 'vertical-border nil t)
  ;;  )

  ;; global built-in modes (graphical)
  (when (display-graphic-p)
    (setq frame-resize-pixelwise t)
    (setq scroll-conservatively 101) ; > 100
    (setq scroll-preserve-screen-position t)
    (setq auto-window-vscroll nil)
    (setq inhibit-compacting-font-caches t)
    (setq echo-keystrokes 0.02)

    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (mac-auto-operator-composition-mode 1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)

    (when (member "Berkeley Mono" (font-family-list))
      (set-frame-font "Berkeley Mono-15:weight=bold")

      ;; start every frame fullscreen
      (add-to-list 'default-frame-alist '(fullscreen))
      )

    (setq
     mac-option-key-is-meta t
     mac-command-key-is-meta nil
     mac-command-modifier 'none
     mac-option-modifier 'meta)

    (add-hook 'window-setup-hook 'toggle-frame-fullscreen)
    ;; (toggle-frame-fullscreen )
    )

  (add-to-list
   'custom-theme-load-path
   (format "%s/themes" user-emacs-directory))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror 'nomessage)

  (setq-default visible-bell nil)
  (setq-default ring-bell-function #'ignore)
  )

;;
;; terminal
;;

(use-package vterm
  :ensure t)

;;
;; `smart-split`
;;

(use-package smart-split
  :straight (:type built-in)
  :load-path "lib"
  :demand t
  :hook (after-init . smart-split))

;; global key bindings including utility functions
(use-package general
  :after format-all
  :ensure t

  :config
  (defun what-face (pos)
    "Display the face at POS."
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))

  (defun show-current-file ()
    "Print the current buffer filename to the minibuffer."
    (interactive)
    (message (buffer-file-name)))

  (defun kill-current-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

  (general-define-key
   "C-j"    'newline-and-indent
   "M-N"    'windmove-right
   "M-P"    'windmove-left
   "C-c f"  'show-current-file
   "C-c q"  'join-line
   "C-c r"  'revert-buffer
   "C-x k"  'kill-current-buffer
   "M-/"    'undo
   "M-z"    'format-all-region-or-buffer
   ))

;;
;; `format-all`
;;

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters '(("Shell" (shfmt "-i" "4" "-ci")))))

;;
;; `company`
;;

(use-package company
  :init
  (global-unset-key (kbd "C-M-i"))

  :config
  (setq company-idle-delay 0.0)
  (global-company-mode)
  (bind-key* "C-M-i" 'company-complete-common-or-cycle))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)

  :config
  (add-to-list
   'company-box-frame-parameters
   '(font . "Berkeley Mono-16:weight=bold")))

;;
;; `doom-modeline.el`
;;

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-minor-modes nil)  ; Changed to nil for a cleaner look
  (setq doom-modeline-lsp t)
  :hook (after-init . doom-modeline-mode))


(use-package all-the-icons
  :ensure t)

(use-package treemacs-all-the-icons
  :ensure t)

(use-package treemacs
  :after all-the-icons
  :ensure t

  :config
  (treemacs-load-theme "all-the-icons"))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic nil) ; if nil, italics is universally disabled

;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users

;;   (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme

;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(use-package ono-sendai-hyper-modern-theme
  :after autothemer
  
  :straight (:type built-in)
  :demand t

  :init
  (load-theme 'ono-sendai-hyper-modern :no-confirm))

;; Load fzf
(use-package fzf
  :ensure t)

;;
;; completion
;;

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package lusty-explorer
  :ensure t
  :config
  (lusty-explorer-mode))

;;
;; paredit
;;

(use-package paredit
  :ensure t
  :config
  (paredit-mode))

(use-package paredit-everywhere
  :after paredit
  :ensure t
  :hook (prog-mode . paredit-everywhere-mode))

;;
;; treesit-auto
;;

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;
;; `eglot`
;;

(use-package eglot
  :ensure t
  :config

  (add-to-list 'eglot-server-programs '(java-ts-mode . ("java-language-server")))
  :hook (java-ts-mode-hook . eglot-ensure)
  :hook (python-ts-mode-hook . eglot-ensure)
  :hook (c++-ts-mode-hook . eglot-ensure))

;;
;; markdown
;;

(use-package markdown-mode
  :ensure t)

;;
;; protocol buffers
;;

(use-package protobuf-mode
  :ensure t)

;;
;; `nix` support
;;

(use-package nixpkgs-fmt
  :ensure t
  )

(use-package nix-mode
  :after nixpkgs-fmt
  :ensure t
  :bind ("M-z" . nixpkgs-fmt-command)
  )

;;
;; `bazel` support
;;

(use-package starlark-format
  :straight (:type built-in)
  :load-path "lib"
  :demand t

  :config
  (require 'starlark-format))

(use-package bazel
  :after (starklark-format)
  :bind (:map bazel-build-mode-map ("M-z" . starlark-format))
  )

;;
;; `java` support
;;

(use-package google-java-format
  :straight (:type built-in)
  :load-path "lib"
  :demand t

  :config
  (require 'google-java-format))

(use-package java-ts-mode
  :after (google-java-format)
  :bind (:map java-ts-mode-map ("M-z" . google-java-format-buffer)))

;;
;; `gradle`
;;

(use-package gradle-mode
  :ensure t)

;;
;; `swift` support
;;

(use-package swift-format
  :straight (:type built-in)
  :load-path "lib"
  :demand t

  :config
  (require 'swift-format))

(use-package swift-mode
  :after (swift-format)
  :bind (:map swift-mode-map ("M-z" . swift-format-buffer)))

;;
;; `typescript` support
;;

(use-package prettier
  :config
  (require 'prettier))

;; (use-package emacs-prisma-mode
;;   :straight (emacs-prisma-mode :type git :host github :repo "pimeys/emacs-prisma-mode.git")
;;   (require 'emacs-prisma-mode))

;; (use-package jtsx-mode
;;   :ensure t
;;   ;; :after prettier
;;   ;; :bind (:map typescript-ts- ("M-z" . prettier-prettify))
;;   )

;;
;; python
;;

;; (use-package py-yapf
;;   :ensure t)

;; (use-package python-mode
;;   :after py-yapf
;;   :ensure t
;;   :config

;;   ;; (setq python-indent-guess-indent-offset (lambda ()))
;;   :bind (:map python-ts-mode ("M-z" . py-yapf-buffer))
;;   :hook (python-ts-mode . (lambda ()
;;                             (setq python-indent-offset 2))))

;;
;; c++ suport
;;

;; (use-package c++-ts-mode
;;   :init
;;   (progn
;;     (require 'c++-ts-mode)
;;     (package-installed-p 'recentf)))

;;
;; unsorted
;;

(use-package rainbow-mode)
(use-package consult)
(use-package autothemer)

;; (defun eglot-eldoc-toggle-order+ ()
;;   "Toggle the precedence of flymake notifications in eldoc."
;;   (unless (bound-and-true-p eglot--managed-mode)
;;     (user-error "Must be called from an `eglot' managed buffer")))

;; this pleasant little snippet unfucks the display of flymake vs. eldoc for c++
;; at least some of the time...
(add-hook
 'eglot-managed-mode-hook
 (lambda ()
   (if (bound-and-true-p eglot--managed-mode)
       (let* ((pos (cl-position #'flymake-eldoc-function eldoc-documentation-functions)))
	       (setq eldoc-documentation-functions
	             (if (eq pos 0)
		               (append (cdr eldoc-documentation-functions) (list #'flymake-eldoc-function))
		             (append (list #'flymake-eldoc-function)
			                   (if (zerop pos)
			                       (cdr eldoc-documentation-functions)
			                     (let ((last (nthcdr (1- pos) eldoc-documentation-functions)))
			                       (setcdr last (cddr last))
			                       eldoc-documentation-functions)))))
	       (message "Message priority: %s"
		              (if (eq pos 0)
		                  (propertize "Documentation" 'face 'compilation-info)
		                (propertize "Errors" 'face 'compilation-error)))))))
