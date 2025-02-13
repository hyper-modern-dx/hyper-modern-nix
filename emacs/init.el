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

;;
;; `emacs` memory management prelude (inspired by @ianyepan)
;;

(defvar file-name-handler-alist-original file-name-handler-alist)

(setq
 gc-cons-threshold        most-positive-fixnum
 gc-cons-percentage       0.6
 file-name-handler-alist  nil
 site-run-file            nil)

(defvar hyper-modern/gc-cons-threshold (* 256 1024 1024))

(add-hook
 'emacs-startup-hook
 (lambda ()

   (defvar compat-30-file (expand-file-name "lib/compat-30.el" user-emacs-directory))
   (load compat-30-file 'noerror 'nomessage)

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

(defvar b7r6/ssh-key-name "id_ed25519_b7r6")

(call-process-shell-command
 (format "ssh-add --apple-use-keychain ~/.ssh/%s" b7r6/ssh-key-name))

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

(defun hyper-modern/setup-backup-and-autosave-directories ()
  "Set up Emacs backup and auto-save directories."

  (let* ((cache-dir (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name "~/.cache")))
         (emacs-cache-dir (expand-file-name "emacs" cache-dir))
         (backup-dir (expand-file-name "backups" emacs-cache-dir))
         (auto-save-dir (expand-file-name "auto-saves" emacs-cache-dir)))

    (dolist (dir (list backup-dir auto-save-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)))

    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,auto-save-dir t))
          auto-save-list-file-prefix (expand-file-name "saves-" auto-save-dir))

    (setq create-lockfiles nil)
    ))

(use-package emacs
  :ensure nil

  :preface
  ;; TODO(b7r6): support multiple profiles/configs based on `$USER`...
  (defvar b7r6/indent-width 2)
  (defvar b7r6/max-columns 80)

  ;; (defvar b7r6/font-family "BerkeleyMono Nerd Font Mono")
  ;; (defvar b7r6/font-height 150)
  ;; (defvar b7r6/font-weight 'semibold)

  ;; (defvar b7r6/posframe-width 128)
  ;; (defvar b7r6/posframe-height 16)
  ;; (defvar b7r6/completion-count 16)

  :init

  :config
  ;; TODO(b7r6): support multiple profiles/configs based on `$USER`...
  (setq user-full-name "b7r6")
  (setq-default default-directory "~/src")

  ;; indent width
  (setq-default indent-tabs-mode nil)               ; use spaces instead of tabs
  (setq-default tab-width        b7r6/indent-width) ; set width for automatic tabs
  (setq-default c-basic-offset   b7r6/indent-width) ; set offset for languages using C style indentation
  (setq-default standard-indent  b7r6/indent-width) ; set default number of spaces for indentation

  ;; hyper modern global defautls
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
  (setq select-enable-clipboard t)

  ;; random files in my dirs, that's the shit i don't like...
  (hyper-modern/setup-backup-and-autosave-directories)


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

  (put 'upcase-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; no pipes in vertical border
  (set-display-table-slot
   standard-display-table 'vertical-border ?│)

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
  (setq-default ring-bell-function #'ignore)

  ;; TODO(b7r6): there's a bigger story around `jvm` and `android` configuration here...
  ;; (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk-17.jdk/Contents/Home")
  )

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
   "C-c d"   'dashboard-open
   "C-j"     'newline-and-indent

   ;; frame mainpulation

   "C-x C-+" 'text-scale-increase
   "C-x C--" 'text-scale-decrease
   "C-x C-r" 'rg-dwim-project-dir
   "C-x d"   'consult-recent-file
   "C-x f"   'consult-fd

   ;; `b7r6` standard keys

   "M-/"     'undo
   "M-N"     'windmove-right
   "M-P"     'windmove-left
   "M-i"     'hyper-modern/visit-init-file
   "M-z"     'format-all-region-or-buffer

   ;; `hyper-modern` overrides

   "C-M-r"   'consult-ripgrep
   "M-R"     'hyper-modern/rotate-windows
   "C-c f"   'hyper-modern/show-current-file
   "C-x 2"   'hyper-modern/vsplit
   "C-x 3"   'hyper-modern/hsplit
   "C-x k"   'hyper-modern/kill-current-buffer

   ;; TODO(b7r6): move to specific mode once we're happy...
   ;; `gptel` bindings
   "C-c a" 'gptel
   "C-c s" 'gptel-menu
   ))

;;
;; `ono-sendai-hyper-modern-theme.el`
;;

(use-package ono-sendai-hyper-modern-theme
  :after autothemer
  :demand t
  :straight (:type built-in)

  :init
  (load-theme 'ono-sendai-hyper-modern :no-confirm)

  :config
  ;; (mapc
  ;;  (lambda (face)
  ;;    (set-face-attribute face nil :weight b7r6/font-weight :family b7r6/font-family))
  ;;  (face-list))
  )

;;
;; completion
;;

(use-package consult
  :ensure t)

(use-package all-the-icons-completion
  :ensure t)

(use-package fzf
  :ensure t)

(use-package vertico
  :ensure t

  :custom
  (vertico-cycle t)

  :init
  (vertico-mode)
  (vertico-reverse-mode)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((command (styles orderless)))))

(use-package posframe
  :ensure t)

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

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

;; (use-package company-box
;;   :after company
;;   :hook (company-mode . company-box-mode)

;;   :config

;;   ;; TODO(b7r6): integrate with global font/face handling...
;;   (add-to-list
;;    'company-box-frame-parameters
;;    '(font . "BerkeleyMono Nerd Font Mono-16:weight=bold"))
;;   )

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
             :repo "zbelial/shrink-path.el"))

;; (use-package shrink-path
;;   :ensure t
;;   :demand t)

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
  (setq doom-modeline-unicode-fallback t)

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

  (dashboard-startup-banner "~/src/hyper-modern/logos/hyper-modern-ascii-logo.txt")

  (dashboard-setup-startup-hook)

  :hook (after-init . dashboard-open))

;;
;; `dirvish`
;;

(use-package dirvish
  :ensure t)

;;
;; `paredit`
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
  (delete 'c++ treesit-auto-langs)

  :config
  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  ;; (global-treesit-auto-mode)
  )

;;
;; `eglot` / `eldoc`
;;

(use-package eglot
  :ensure t

  :config
  (setq eglot-stay-out-of '("flymake" "company"))

  :hook (eglot-managed-mode . (lambda ()
                                (eglot-inlay-hints-mode -1)
                                (flymake-mode -1)))
  :hook (prog-mode . eglot-ensure)
  :hook (prog-mode . eldoc-mode))

;; (use-package eldoc-box
;;   :ensure t
;;   :hook (prog-mode . eldoc-box-hover-at-point-mode))

;;
;; `magit`
;;

(use-package magit
  :ensure t)

;;
;; compilation
;;

(use-package compile
  :ensure t

  :config
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  )

;;
;; `protocol buffers` support
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
;; `c++` support
;;

(use-package clang-format
  :ensure t
  )

;; (use-package cc-mode
;;   :ensure t
;;   :mode ("\\.cpp\\'" "\\.h\\'")

;;   ;; (add-hook 'cc-mode-hook (lambda () (c++-mode)))

;;   ;; :config
;;   ;; :bind (:map c++-mode-map ("M-z" . clang-format-buffer))
;;   )

;; (setq c++-mode c++-mode)
;; (add-hook 'c++-mode-hook '(lambda () (message "c++ in the house")))


(use-package cmake-mode
  :ensure t)

;;
;; `python` support
;;

(use-package py-isort
  :ensure t
  :after python)

(use-package yapfify
  :ensure t
  :after python)

(use-package ruff-format
  :ensure t
  )

(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.pconf\\'" . python-mode)
         ("\\.pinc\\'" . python-mode)
         ("\\.proto-validator\\'" . python-mode))

  :config
  :bind (:map python-mode-map
              ("M-z" . (lambda ()
                         (interactive)
                         (ruff-format-buffer)))))

;;
;; `java` support
;;

(use-package java-ts-mode
  :after eglot
  :ensure t

  :config
  (setq java-mode-indent-offset b7r6/indent-width)

  (add-to-list
   'eglot-server-programs
   '(java-mode . ("java-language-server")))

  :hook (java-mode . (lambda () (setq format-all-formatters '(("Java" (clang-format))))))
  :mode "\\.java\\'")

;;
;; `kotlin` support
;;

(use-package hyper-modern-ktlint-format
  :straight (:type built-in)
  :load-path "lib"
  :after emacs)

;; (use-package kotlin-mode
;;   :after hyper-modern-ktlint-format
;;   :ensure t

;;   :mode ("\\.kt\\'" . kotlin-mode)
;;   :mode ("\\.kts?\\'" . kotlin-mode)

;;   :init
;;   (setq kotlin-mode-indent-offset b7r6/indent-width)
;;   (setq kotlin-mode-indent-offset b7r6/indent-width)
;;   (setq kotlin-tab-width b7r6/indent-width)
;;   (setq kotlin-mode-indent-offset b7r6/indent-width)

;;   :config
;;   (add-to-list 'eglot-server-programs '(kotlin-mode . ("kotlin-language-server")))

;;   :bind (:map kotlin-mode-map ("M-z" . hyper-modern/ktlint-format-buffer))

;;   :hook (kotlin-mode . eglot-ensure))

;;
;; `gradle`
;;

(use-package gradle-mode
  :ensure t
  :config
  (push 'gradle compilation-error-regexp-alist)
  (push '(gradle "\(file://\)?\([A-Za-z0-9/-]+.[a-z]+\):\([0-9]+\):\([0-9]+\)" 2 3 4)
        compilation-error-regexp-alist-alist)
  )

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

(use-package json-ts-mode
  :after prettier
  :ensure t

  :hook
  (json-ts-mode . (lambda () (setq format-all-formatters '(("JSON" (prettier)))))))


(use-package typescript-ts-mode
  :after prettier
  :ensure t

  :config
  ;; (setq typescript-mode-indent-offset b7r6/indent-width)
  (setq typescript-indent-level 2)     ; for typescript-mode
  (setq tsx-indent-level 2)            ; for tsx-mode

  (defun pnpm-lint ()
    (interactive)
    (compilation-start "pnpm lint" 'compilation-mode))

  (defun pnpm-build ()
    (interactive)
    (compilation-start "pnpm build" 'compilation-mode))

  :hook
  (typescript-ts-mode . (lambda () (setq format-all-formatters '(("TypeScript" (prettier))))))
  (tsx-ts-mode . (lambda () (setq format-all-formatters '(("TSX" (prettier)))))))

;; (use-package javascript-ts-mode
;;   :after prettier
;;   :ensure t
;;   :hook
;;   (javascript-ts-mode . (lambda () (setq format-all-formatters '(("JavaScript" (prettier))))))
;;   (jsx-ts-mode . (lambda () (setq format-all-formatters '(("JSX" (prettier)))))))

;;
;; `haskell-mode`
;;

(use-package haskell-mode
  :ensure true)

;;
;; `ruby-mode`
;;

(use-package ruby-mode
  :ensure true)

;;
;; `markdown`
;;

(use-package markdown-mode
  :ensure t)

;;
;; `docker`
;;

(use-package dockerfile-mode
  :ensure t)

;;
;; `yaml-mode`
;;

(use-package yaml-mode
  :ensure t)

;;
;; `mustache-mode`
;;

(use-package mustache-mode
  :ensure t)

;;
;; `lua` support
;;

(use-package lua-mode
  :ensure t)

;;
;; `rainbow-mode`
;;

(use-package rainbow-mode
  :ensure t)

;;
;; `autothemer`
;;

(use-package autothemer
  :ensure t)

;;
;; `fontify-face`
;;

(use-package fontify-face
  :ensure t)

;;
;; `csv-mode`
;;

(use-package csv-mode
  :ensure t)

;;
;; `gptel`
;;

(use-package gptel
  :ensure t
  :config
  (setq gptel-max-tokens (* 8 1024)
        gptel-response-length (* 4 1024))

  (setq gptel-backend
        (gptel-make-openai "deepseek"
          :stream t
          :models '(deepseek-chat
                   deepseek-coder
                   deepseek-reasoner)
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :key "sk-be77179fc2e141a98dae94d3ed2aa73d"))

  (gptel-make-anthropic "sonnet"
    :stream t
    :key "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"
    :models '(claude-3-5-sonnet-20241022))

  (gptel-make-gemini "gemini"
    :stream t
    :key "AIzaSyD94WNHl0Bd4a8F80g63m4J9rnekBGc8X4"))

(defcustom hyper-modern/gptel-buffer-name "*HYPER // MODERN // AI*"
  "Name of the buffer used for GPTel interactions."
  :type 'string
  :group 'hyper-modern)

(defvar hyper-modern/gptel-response-timer nil
  "Timer for checking GPTel response.")

(defvar hyper-modern/gptel-last-point nil
  "Last known maximum point in the GPTel buffer.")

(defvar hyper-modern/gptel-stable-count 0
  "Counter for how many checks the buffer size has remained stable.")

(defvar hyper-modern/gptel-prompts
  '(("Review" . "As an expert programmer, review the following code. Focus on efficiency, readability, and adherence to best practices. Provide concise, actionable feedback:\n\n")
    ("Refactor" . "Refactor the following code to improve its structure and efficiency. Maintain its functionality while enhancing readability and performance. Explain your changes:\n\n")
    ("Optimize" . "Analyze this code for performance bottlenecks and suggest optimizations. Consider time complexity, memory usage, and any language-specific optimizations:\n\n")
    ("Explain" . "Provide a clear, concise explanation of what this code does. Break down complex parts and highlight any notable patterns or algorithms used:\n\n")
    ("Debug" . "Examine this code for potential bugs or edge cases. Suggest fixes and explain your reasoning. If you see no bugs, it's fine to just say that.:\n\n")
    ("Modernize" . "Update this code to use more modern language features and idioms. Explain how these changes improve the code:\n\n")
    ("Document" . "Generate comprehensive documentation for this code. Include function purposes, parameters, return values, and any important implementation details:\n\n")
    ("Test" . "Propose a set of unit tests for this code. Cover main functionality, edge cases, and potential failure modes:\n\n")
    ("Custom" . ""))
  "Alist of optimized prompts for Sonnet 3.5 interactions.")


(defun hyper-modern/get-language-from-mode ()
  "Get the language name from the current major mode."
  (let ((mode-name (symbol-name major-mode)))
    (if (string-match "\\(.*\\)-mode$" mode-name)
        (match-string 1 mode-name)
      mode-name)))

(defun hyper-modern/get-region-content ()
  "Get the content of the selected region."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (user-error "No region selected")
    ))


(defun hyper-modern/format-content (content language)
  "Format CONTENT with language-specific code blocks for LANGUAGE."
  (format "```%s\n%s\n```" language content))

(defun hyper-modern/get-or-create-gptel-buffer ()
  "Get or create the GPTel buffer."
  (or (get-buffer hyper-modern/gptel-buffer-name)
      (gptel hyper-modern/gptel-buffer-name)))

(defun hyper-modern/post-gptel-response (response-begin response-end)
  "Function to run after `gptel` response is complete."
  (let ((gptel-buffer (or (hyper-modern/get-or-create-gptel-buffer)
                          (user-error "Failed to create or get gptel buffer"))))

    (display-buffer gptel-buffer '(display-buffer-same-window))
    (when (buffer-local-value 'hyper-modern/gptel-recentering-needed (current-buffer))
      (goto-char response-end)
      (recenter -1)
      (setq-local hyper-modern/gptel-recentering-needed nil))
    ))

(add-hook 'gptel-post-response-functions #'hyper-modern/post-gptel-response)

(defun hyper-modern/go-to-gptel-buffer ()
  "Function to run after `gptel` response is complete."
  (interactive)
  (let ((gptel-buffer (or (hyper-modern/get-or-create-gptel-buffer)
                          (user-error "Failed to create or get gptel buffer"))))

    (display-buffer gptel-buffer '(display-buffer-same-window))
    ))

(defun hyper-modern/gptel-send-region (prompt-key)
  "Send the selected region to gptel with a specified prompt.
PROMPT-KEY is a key in `hyper-modern/gptel-prompts'."
  (interactive
   (list (completing-read "Choose prompt: "
                          (mapcar #'car hyper-modern/gptel-prompts))))
  (unless (boundp 'hyper-modern/gptel-prompts)
    (user-error "hyper-modern/gptel-prompts is not defined"))

  (unless (region-active-p)
    (user-error "No active region"))

  (unless (fboundp 'gptel-send)
    (user-error "gptel is not available"))

  (let* ((region-content (hyper-modern/get-region-content))
         (language (hyper-modern/get-language-from-mode))
         (prompt (or (cdr (assoc prompt-key hyper-modern/gptel-prompts))
                     prompt-key))
         (formatted-content (hyper-modern/format-content region-content language))
         (gptel-buffer (or (hyper-modern/get-or-create-gptel-buffer)
                           (user-error "Failed to create or get gptel buffer")))
         (full-prompt (concat prompt "\n\n" formatted-content))
         (original-window (selected-window)))

    ;; (display-buffer gptel-buffer '(display-buffer-same-window))
    (display-buffer gptel-buffer)
    (with-current-buffer gptel-buffer
      (goto-char (point-max))
      (insert full-prompt)
      (setq-local hyper-modern/gptel-recentering-needed t)
      (gptel-send)
      (goto-char (point-max))
      ;; (recenter-top-bottom 'center)
      )

    (message "HYPER // MODERN request sent with prompt: %s" prompt-key)
    ))

(defun hyper-modern/gptel-send-region-review ()
  "Send the selected region to gptel for review."
  (interactive)
  (hyper-modern/gptel-send-region "Review"))

(defun hyper-modern/gptel-send-region-refactor ()
  "Send the selected region to gptel for refactoring suggestions."
  (interactive)
  (hyper-modern/gptel-send-region "Refactor"))

(defun hyper-modern/gptel-send-region-optimize ()
  "Send the selected region to gptel for optimization suggestions."
  (interactive)
  (hyper-modern/gptel-send-region "Optimize"))

(defun hyper-modern/gptel-send-region-explain ()
  "Send the selected region to gptel for explanation."
  (interactive)
  (hyper-modern/gptel-send-region "Explain"))

(defun hyper-modern/gptel-send-region-debug ()
  "Send the selected region to gptel for debugging."
  (interactive)
  (hyper-modern/gptel-send-region "Debug"))

(defun hyper-modern/gptel-send-region-modernize ()
  "Send the selected region to gptel for modernization suggestions."
  (interactive)
  (hyper-modern/gptel-send-region "Modernize"))

(defun hyper-modern/gptel-send-region-document ()
  "Send the selected region to gptel for documentation generation."
  (interactive)
  (hyper-modern/gptel-send-region "Document"))

(defun hyper-modern/gptel-send-region-test ()
  "Send the selected region to gptel for test case suggestions."
  (interactive)
  (hyper-modern/gptel-send-region "Test"))

(defun hyper-modern/gptel-send-region-custom ()
  "Send the selected region to gptel with a custom prompt."
  (interactive)
  (let ((custom-prompt (read-string "Enter custom prompt: ")))
    (hyper-modern/gptel-send-region custom-prompt)))

(defvar hyper-modern/ai-interface-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") #'hyper-modern/go-to-gptel-buffer)
    (define-key map (kbd "r") #'hyper-modern/gptel-send-region-review)
    (define-key map (kbd "f") #'hyper-modern/gptel-send-region-refactor)
    (define-key map (kbd "o") #'hyper-modern/gptel-send-region-optimize)
    (define-key map (kbd "e") #'hyper-modern/gptel-send-region-explain)
    (define-key map (kbd "d") #'hyper-modern/gptel-send-region-debug)
    (define-key map (kbd "m") #'hyper-modern/gptel-send-region-modernize)
    (define-key map (kbd "c") #'hyper-modern/gptel-send-region-document)
    (define-key map (kbd "t") #'hyper-modern/gptel-send-region-test)
    (define-key map (kbd "u") #'hyper-modern/gptel-send-region-custom)
    map)
  "Keymap for hyper-modern GPTel commands.")

(global-set-key (kbd "C-c c") hyper-modern/ai-interface-keymap)

;;
;; `tree-sitter` grammar configuration
;; TODO(b7r6): get this cleaned up...
;;

(setq
 treesit-language-source-alist
 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   (cmake "https://github.com/uyha/tree-sitter-cmake")
   (css "https://github.com/tree-sitter/tree-sitter-css")
   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (html "https://github.com/tree-sitter/tree-sitter-html")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun ensure-treesit-languages ()
  "Ensure all languages in treesit-language-source-alist are installed."
  (dolist (lang-source treesit-language-source-alist)
    (let ((lang (car lang-source)))
      (unless (treesit-language-available-p lang)
        (message "Installing tree-sitter grammar for %s" lang)
        (treesit-install-language-grammar lang)))))

(when (treesit-available-p)
  (ensure-treesit-languages))

;;
;; `svg-tag-mode`
;;

;;
;; TODO(b7r6): get this working again...
;;
;; (use-package svg-tag-mode
;;   :ensure t
;;   :init  ; Use :init instead of :config to ensure it runs at startup
;;   (setq svg-tag-tags
;;         '(("TODO(b7r6):" .
;;            ((lambda (tag)
;;               (svg-tag-make "TODO" :inverse t :radius 0 :face 'font-lock-comment-face))))))
;;   :config
;;   (svg-tag-mode 1))  ; Enable the mode at startup

;; TODO(b7r6): debug this...
;;
;; (use-package current-window-only
;;   :straight (current-window-only
;;              :type git
;;              :host github
;;              :repo "FrostyX/current-window-only")
;;   :config
;;   (current-window-only-mode))
