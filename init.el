;;
;; `hyper-modern`
;;

;;
;; "On The Design Of Text Editors" - https://arxiv.org/abs/2008.06030
;;

;;
;;
;; “There is always a point at which the terrorist ceases to manipulate the media
;;  gestalt. A point at which the violence may well escalate, but beyond which the terrorist
;;  has become symptomatic of the media gestalt itself. Terrorism as we ordinarily
;;  understand it is inately media-related. The Panther Moderns differ from other
;;  terrorists precisely in their degree of self-consciousness, in their awareness of
;;  the extent to which media divorce the act of terrorism from the original sociopolitical
;;  intent.”
;;
;; “Skip it.” Case said.
;;

;;
;; `prelude`
;;

;; `emacs` memory management tuning (inspired by @ianyepan)
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
  (defvar b7r6/ssh-key-name "id_ed25519_b7r6")
  (defvar b7r6/indent-width 2)
  (defvar b7r6/max-columns 100)

  :init
  ;; `ssk-agent` support for github packages
  (shell-command
   (format "ssh-add --apple-use-keychain ~/.ssh/%s" b7r6/ssh-key-name))
  (setq straight-vc-git-default-protocol 'ssh)

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

    ;; TODO(b7r6): this is godawful, do something about it...
    (add-hook
     'window-setup-hook
     (lambda ()
       (toggle-frame-fullscreen))))

  (add-to-list
   'custom-theme-load-path
   (format "%s/themes" user-emacs-directory))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror 'nomessage)

  (setq-default visible-bell nil)
  (setq-default ring-bell-function #'ignore)

  (defadvice window-configuration-change-hook
      (before switch-to-scratch activate)
    "Switch to *scratch* buffer in the new window."
    (let ((default-directory (buffer-file-name (current-buffer))))
      (if (not (get-buffer "*scratch*"))
          (progn
            (find-file "*scratch*")
            (setq default-directory (buffer-file-name (current-buffer)))))))

  (defun switch-to-scratch ()
    "Switch to *scratch* buffer."
    (interactive)
    (if (get-buffer "*scratch*")
        (switch-to-buffer "*scratch*")))

  ) ;; (use-package emacs)

(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; terminal
;;

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil))))

;;
;; `smart-split`
;;

(use-package smart-split
  :straight (:type built-in)
  :load-path "lib"
  :after emacs)

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

  (defun visit-init-file ()
    (interactive)
    (find-file user-init-file)

    )

  (general-define-key
   "C-c f"   'show-current-file
   "C-c q"   'join-line
   "C-c r"   'revert-buffer
   "C-j"     'newline-and-indent
   "C-x C-+" 'text-scale-increase
   "C-x C--" 'text-scale-decrease
   "C-x f"   'toggle-frame-fullscreen
   "C-x k"   'kill-current-buffer
   "C-x C-r" 'rg-dwim-project-dir
   "M-/"     'undo
   "M-N"     'windmove-right
   "M-P"     'windmove-left
   "M-i"     'visit-init-file
   "M-z"     'format-all-region-or-buffer
   ))

;;
;; `format-all`
;;

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  ;; (setq-default format-all-formatters '(("Shell" (shfmt "-i" "4" "-ci"))))
  )

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
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :hook (prog-mode . eglot-ensure)
  :hook (prog-mode . eldoc-mode))

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

(use-package java-ts-mode
  :after eglot
  :ensure t
  :config
  (setq java-ts-mode-indent-offset b7r6/indent-width)
  :hook (java-ts-mode . (lambda () (setq format-all-formatters '(("Java" (clang-format)))))))

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
  :ensure t)

(straight-use-package
 '(compile-eslint :type git :host github :repo "Fuco1/compile-eslint"))

(use-package compile-eslint
  :ensure t
  :config
  (push 'eslint compilation-error-regexp-alist))

(use-package typescript-ts-mode
  :after prettier
  :ensure t

  :config
  (setq typescript-ts-mode-indent-offset b7r6/indent-width)

  (defun pnpm-lint ()
    (interactive)
    (compilation-start "pnpm lint" 'compilation-mode))

  (defun pnpm-build ()
    (interactive)
    (compilation-start "pnpm build" 'compilation-mode))

  :hook
  (typescript-ts-mode . (lambda () (setq format-all-formatters '(("TypeScript" (prettier))))))
  (tsx-ts-mode . (lambda () (setq format-all-formatters '(("TSX" (prettier)))))))

;;
;; unsorted
;;

(use-package prisma-mode
  :after eglot
  :straight (prisma-mode
             :type git
             :host github
             :repo "davidarenas/prisma-mode")
  :config
  (add-to-list 'eglot-server-programs '(prisma-mode . ("prisma-language-server"))))

(use-package llama-cpp
  :ensure t)

(use-package magit
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package consult
  :ensure t)

(use-package autothemer
  :ensure t)

(use-package fontify-face
  :ensure t)

(use-package current-window-only
  :straight (current-window-only
             :type git
             :host github
             :repo "FrostyX/current-window-only")
  :config
  (current-window-only-mode))

(use-package rg
  :ensure t
  :config
  ;; Set default directory to search in
  (setq rg-default-directory (expand-file-name "~/src"))

  ;; Use ripgrep as the default search tool in Projectile
  (setq projectile-use-rg t)

  ;; Group search results by file
  (setq rg-group-result t)

  ;; Context lines: 2 lines before and after the match
  (setq rg-context-line-count 2)

  ;; Show search results in a new window
  (setq rg-show-columns t)

  ;; Keybindings
  :bind (("C-c C-r" . rg)
         ("C-c s p" . rg-project)
         ("C-c s d" . rg-dwim)
         ("C-c s l" . rg-list-searches)))

;; TODO(b7r6): think of more awesome tags to put in here...
;; TODO(everyone-else): consider living the `hyper-modern` life...
(use-package svg-tag-mode
  :ensure t
  :config
  (setq
   svg-tag-tags
   '(("TODO(b7r6):" . ((lambda (tag) (svg-tag-make "TODO" :inverse t :radius 0 :face 'font-lock-comment-face)))))))
