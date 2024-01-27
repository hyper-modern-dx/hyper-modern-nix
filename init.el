;;; init.el --- -*- lexical-binding: t -*-

;;
;; `hyper-modern`
;;

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

;;
;; `emacs` memory management prelude (inspired by @ianyepan)
;;

(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold        most-positive-fixnum
      gc-cons-percentage       0.6
      file-name-handler-alist  nil
      site-run-file            nil)

(defvar hyper-modern/gc-cons-threshold (* 256 1024 1024))

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold hyper-modern/gc-cons-threshold
         gc-cons-percentage 0.1
         file-name-handler-alist file-name-handler-alist-original)))

(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (setq gc-cons-threshold most-positive-fixnum)))

(add-hook
 'minibuffer-exit-hook
 (lambda ()
   (garbage-collect)
   (setq gc-cons-threshold hyper-modern/gc-cons-threshold)))

;;
;; `straight.el` bootstrap prelude
;;

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

(setq straight-vc-git-default-protocol 'ssh
      straight-use-package-by-default  t)

;;
;; global `emacs` settings
;;

(use-package emacs
  :ensure nil

  :preface
  (defvar b7r6/ssh-key-name "id_ed25519_b7r6")
  (defvar b7r6/indent-width 2)
  (defvar b7r6/max-columns 100)

  (defvar b7r6/font-family "Berkeley Mono")
  (defvar b7r6/font-height 155)
  (defvar b7r6/font-weight 'semibold)

  (defvar b7r6/posframe-width 128)
  (defvar b7r6/posframe-height 16)
  (defvar b7r6/completion-count 16)

  :init
  (shell-command
   (format "ssh-add --apple-use-keychain ~/.ssh/%s" b7r6/ssh-key-name))

  :config
  (setq user-full-name "b7r6")
  (setq-default default-directory "~/src")

  ;; indent width
  (setq-default indent-tabs-mode nil)               ; Use spaces instead of tabs
  (setq-default tab-width        b7r6/indent-width) ; Set width for automatic tabs
  (setq-default c-basic-offset   b7r6/indent-width) ; Set offset for languages using C style indentation
  (setq-default standard-indent  b7r6/indent-width) ; Set default number of spaces for indentation

  ;; modern global defautls
  (setq auto-save-default               nil)
  (setq confirm-kill-processes          nil)
  (setq debug-on-error                  nil)
  (setq echo-keystrokes                 0.1)
  (setq indent-tabs-mode                nil)
  (setq inhibit-startup-message           t)
  (setq initial-scratch-message          "")
  (setq make-backup-files               nil)
  (setq pop-up-windows                  nil)
  (setq require-final-newline             t)
  (setq resize-mini-windows             nil)
  (setq ring-bell-function          'ignore)
  (setq scroll-conservatively         10000)
  (setq scroll-step                       1)
  (setq select-enable-clipboard           t)
  (setq split-height-threshold          nil)
  (setq split-width-threshold           nil)
  (setq transient-mark-mode               t)
  (setq cursor-in-non-selected-windows  nil)
  (setq backup-by-copying                 t)

  ;; global built-in modes
  (blink-cursor-mode       +1)
  (column-number-mode      +1)
  (display-time-mode       +1)
  (flymake-mode            -1)
  (global-auto-revert-mode +1)
  (global-hl-line-mode     +1)
  (global-whitespace-mode  -1)
  (menu-bar-mode           -1)
  (show-paren-mode         +1)

  (fset 'yes-or-no-p 'y-or-n-p)

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
    (scroll-bar-mode -1)
    (tool-bar-mode -1)

    (when (member b7r6/font-family (font-family-list))
      (set-face-attribute 'default nil :font b7r6/font-family :height b7r6/font-height :weight b7r6/font-weight)
      (set-face-attribute 'fixed-pitch nil :font b7r6/font-family :height b7r6/font-height :weight b7r6/font-weight)
      (set-face-attribute 'fixed-pitch-serif nil :font b7r6/font-family :height b7r6/font-height :weight b7r6/font-weight)
      (set-face-attribute 'variable-pitch nil :font b7r6/font-family :height b7r6/font-height :weight b7r6/font-weight)

      ;; start every frame fullscreen
      (add-to-list 'default-frame-alist '(fullscreen)))

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

  ;; TODO(b7r6): fix this...
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq-default visible-bell nil)
  (setq-default ring-bell-function #'ignore))

;;
;; `vterm.el`
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

;;
;; `reformatter`
;;

(use-package reformatter
  :ensure t)

;;
;; `format-all`
;;

(use-package format-all
  :straight t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode))

;;
;; key bindings and associated utilities
;;

(use-package which-key
  :custom
  (which-key-idle-delay 2)
  :config
  (which-key-mode))

(use-package general
  :after format-all
  :ensure t

  :config

  (defun hyper-modern/what-face (pos)
    "Display the face at POS."
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))

  (defun hyper-modern/show-current-file ()
    "Print the current buffer filename to the minibuffer."
    (interactive)
    (message (buffer-file-name)))

  (defun hyper-modern/kill-current-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

  (defun hyper-modern/visit-init-file ()
    (interactive)
    (find-file user-init-file)
    )

  (defun hyper-modern/rotate-windows ()
    (interactive)
    (let* ((original-buffer (current-buffer))
           (windows (window-list))
           (buffers (mapcar 'window-buffer windows))
           (num-windows (length windows)))
      (when (> num-windows 1)
        (dotimes (i num-windows)
          (set-window-buffer (nth i windows) (nth (mod (+ i 1) num-windows) buffers)))

        (let ((original-window (get-buffer-window original-buffer t)))
          (when original-window
            (select-window original-window))))))

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

  (general-define-key

   ;; standard movement

   "C-c q"   'join-line
   "C-c r"   'revert-buffer
   "C-j"     'newline-and-indent

   ;; frame mainpulation

   "C-x C-+" 'text-scale-increase
   "C-x C--" 'text-scale-decrease
   "C-x C-r" 'rg-dwim-project-dir
   "C-x f"   'toggle-frame-fullscreen

   ;; `b7r6` standard keys

   "M-/"     'undo
   "M-N"     'windmove-right
   "M-P"     'windmove-left
   "M-i"     'hyper-modern/visit-init-file
   "M-z"     'format-all-region-or-buffer

   ;; `hyper-modern` overrides

   "C-M-r"   'hyper-modern/rotate-windows
   "C-c f"   'hyper-modern/show-current-file
   "C-x 2"   'hyper-modern/vsplit
   "C-x 3"   'hyper-modern/hsplit
   "C-x k"   'hyper-modern/kill-current-buffer
   ))

;;
;; `ono-sendai-hyper-modern-theme.el`
;;

(use-package ono-sendai-hyper-modern-theme
  :after autothemer
  :demand t
  :straight (:type built-in)

  :init
  (load-theme 'ono-sendai-hyper-modern :no-confirm))

;;
;; completion
;;

(use-package fzf
  :ensure t)

(use-package amx
  :ensure t
  :config
  (amx-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((command (styles orderless)))))

(use-package posframe
  :ensure t)

(use-package vertico-posframe
  :after vertico posframe
  :ensure t

  :config
  (vertico-posframe-mode +1)

  :custom
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  (vertico-posframe-width b7r6/posframe-width)
  (vertico-posframe-height b7r6/posframe-height)

  (vertico-posframe-poshandler 'posframe-poshandler-frame-center)
  (vertico-count b7r6/completion-count))

;;
;; project/directory searching
;;

(use-package rg
  :ensure t
  :config
  ;; Set default directory to search in
  (setq rg-default-directory (expand-file-name "."))

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

;;
;; `company`
;;

(use-package company
  :init
  (global-unset-key (kbd "C-M-i"))

  :config
  (setq company-idle-delay 0.0)
  (global-company-mode)

  ;; TODO(b7r6): use proper `:bind`...
  (bind-key* "C-M-i" 'company-complete-common-or-cycle))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)

  :config

  ;; TODO(b7r6): integrate with global font/face handling...
  (add-to-list
   'company-box-frame-parameters
   '(font . "Berkeley Mono-16:weight=bold")))

;;
;; `all-the-icons.el`
;;

(use-package all-the-icons
  :ensure t)

;;
;; `doom-modeline.el`
;;

(use-package shrink-path
  :straight (shrink-path
             :type git
             :host github
             :repo "bennya/shrink-path.el"))

(use-package doom-modeline
  :after all-the-icons shrink-path
  :straight (doom-modeline
             :type git
             :host github
             :repo "seagle0128/doom-modeline")

  :config
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-unicode-fallback nil)

  :hook (after-init . doom-modeline-mode))

;;
;; `dashboard.el`
;;

(use-package dashboard-hackernews
  :ensure t)

(use-package projectile
  :ensure t)

(use-package dashboard
  :after (all-the-icons dashboard-hackernews projectile)

  ;; seems like the latest versions do some fuckery with the project list or something.
  ;; Need an extra refresh after initialization for my own settings to show up now.
  ;; (did not need this before. Would rather keep the :custom block instead of setq spamming)
  :custom

  (dashboard-banner-logo-title
   "it was the style that mattered and the style was the same.\nthe moderns were mercenaries, practical jokers, nihilistic tehcnofetishists.")

  (dashboard-center-content t)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   '(((" " "RELOAD // INIT " "" (lambda (&rest _) (load-file user-init-file))))))

  (dashboard-icon-type 'all-the-icons)

  ;; TODO: enable again when they work
  ;;       https://github.com/emacs-dashboard/emacs-dashboard/issues/459
  (dashboard-set-heading-icons nil)


  ;; TODO: see if we can activate the footer again in the future
  ;;       Seems like nil gets sent to the insert function now. Unsure if it happens pre Emacs 29
  ;;       The first element is an icon, so might be related to the other icon issues.
  (dashboard-set-footer nil)

  (dashboard-set-file-icons t)
  (dashboard-items '((projects . 5)
                     (recents . 5)
                     (hackernews . 5)))

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-display-icons-p t)

  (dashboard-startup-banner "~/.emacs.d/hyper-modern-logo.svg")

  (dashboard-setup-startup-hook)

  :hook (after-init . dashboard-open))

;;
;; `treemacs.el`
;;

(use-package treemacs-all-the-icons
  :ensure t)

(use-package treemacs
  :after all-the-icons
  :ensure t

  :config
  (treemacs-load-theme "all-the-icons"))

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
;; `eglot` / `eldoc`
;;

(use-package eglot
  :ensure t

  :config
  (setq eglot-stay-out-of '("flymake" "company"))

  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :hook (prog-mode . eglot-ensure)
  :hook (prog-mode . eldoc-mode))

(use-package eldoc-box
  :ensure t
  :hook (prog-mode . eldoc-box-hover-at-point-mode))

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
  :ensure t)

(use-package nix-mode
  :after nixpkgs-fmt
  :ensure t)

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

  (add-to-list
   'eglot-server-programs
   '(java-ts-mode . ("java-language-server")))

  :hook (java-ts-mode . (lambda () (setq format-all-formatters '(("Java" (clang-format))))))
  :mode "\\.java\\'")

;;
;; `kotlin` support
;;

(use-package hyper-modern-ktlint-format
  :straight (:type built-in)
  :load-path "lib"
  :after emacs)

(use-package kotlin-ts-mode
  :after hyper-modern-ktlint-format
  :ensure t

  :mode ("\\.kt\\'" . kotlin-ts-mode)
  :mode ("\\.kts?\\'" . kotlin-ts-mode)

  :init
  (setq kotlin-mode-indent-offset b7r6/indent-width)
  (setq kotlin-mode-indent-offset b7r6/indent-width)
  (setq kotlin-tab-width b7r6/indent-width)
  (setq kotlin-ts-mode-indent-offset b7r6/indent-width)

  :config
  (add-to-list 'eglot-server-programs '(kotlin-ts-mode . ("kotlin-language-server")))

  :bind (:map kotlin-ts-mode-map ("M-z" . hyper-modern/ktlint-format-buffer))

  :hook (kotlin-mode . eglot-ensure))

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
;; `prisma` support
;;

(use-package prisma-mode
  :after eglot
  :straight (prisma-mode
             :type git
             :host github
             :repo "davidarenas/prisma-mode")
  :config
  (add-to-list 'eglot-server-programs '(prisma-mode . ("prisma-language-server"))))

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

(use-package gptel
  :ensure t)

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

;; TODO(b7r6): finish adding v0.1.0 tags...
(use-package svg-tag-mode
  :ensure t
  :config
  (setq
   svg-tag-tags
   '(("TODO(b7r6):" .
      ((lambda (tag)
         (svg-tag-make "TODO" :inverse t :radius 0 :face 'font-lock-comment-face))))))

  (svg-tag-mode))

(use-package current-window-only
  :straight (current-window-only
             :type git
             :host github
             :repo "FrostyX/current-window-only"))
