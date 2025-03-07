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

   (defvar compat-30-file
     (expand-file-name "lib/compat-30.el" user-emacs-directory))

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
      (set-face-attribute
       'default nil
       :font b7r6/font-family :height b7r6/font-height :weight b7r6/font-weight)
      (set-face-attribute
       'fixed-pitch nil
       :font b7r6/font-family :height b7r6/font-height :weight b7r6/font-weight)
      (set-face-attribute
       'fixed-pitch-serif nil
       :font b7r6/font-family :height b7r6/font-height :weight b7r6/font-weight)
      (set-face-attribute
       'variable-pitch nil
       :font b7r6/font-family :height b7r6/font-height :weight b7r6/font-weight)

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
  )

;;
;; `vterm.el`
;;

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  (setq vterm-keymap-exceptions '("M-/" "M-N" "M-P" "M-i" "M-z"))

  ;; Set up keybindings after vterm is fully loaded
  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "M-N") 'windmove-right)
    (define-key vterm-mode-map (kbd "M-P") 'windmove-left)
    ))

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
          (set-window-buffer
           (nth i windows) (nth (mod (+ i 1) num-windows) buffers)))

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

  :config)

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

  ;; Always search all files in project
  (setq rg-command-line-flags '("--type=all"))

  (defun my-rg-project-prompt ()
    (interactive)
    (let ((current-prefix-arg '(4))) ; Force prompt behavior
      (call-interactively 'rg-project)))

  ;; Unbind M-N and M-P from rg-mode-map
  (with-eval-after-load 'rg
    (define-key rg-mode-map (kbd "M-N") nil)
    (define-key rg-mode-map (kbd "M-P") nil))

  ;; Keybindings
  :bind (("C-c C-r" . my-rg-project-prompt)
         ("C-c s p" . my-rg-project-prompt)
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

  :hook (after-init . doom-modeline-mode)
  )

;;
;; `dashboard.el`
;;

(use-package dashboard-hackernews
  :ensure t)

(use-package projectile
  :ensure t)

(use-package dashboard
  :after (all-the-icons dashboard-hackernews projectile)

  ;; seems like the latest versions do some fuckery with the project list or
  ;; something.
  ;; Need an extra refresh after initialization for my own settings to show up now.
  ;; (did not need this before. Would rather keep the :custom block instead
  ;; of setq spamming)
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


  ;; TODO(b7r6):
  ;; see if we can activate the footer again in the future
  ;; Seems like nil gets sent to the insert function now. Unsure
  ;; if it happens pre Emacs 29
  ;; The first element is an icon, so might be related to the other icon issues.
  ;; (dashboard-set-footer nil)

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
  :ensure t
  :config
  ;; Function to display magit in the rightmost window
  (defun my-magit-display-buffer-function (buffer)
    "Display BUFFER in the rightmost window without splitting."
    (let ((window (if (one-window-p)
                      (selected-window)
                    (window-at (- (frame-width) 2) 1))))
      (select-window window)
      (set-window-buffer window buffer)
      window))

  ;; Set magit to use our custom display function
  (setq magit-display-buffer-function #'my-magit-display-buffer-function))

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
  :bind (:map nix-mode-map
              ("M-z" . nixpkgs-fmt-buffer)))

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

  :hook
  (java-mode . (lambda () (setq format-all-formatters '(("Java" (clang-format))))))

  :mode "\\.java\\'")

;;
;; `kotlin` support
;;

(use-package hyper-modern-ktlint-format
  :straight (:type built-in)
  :load-path "lib"
  :after emacs)

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

  (setq format-all-formatters '(("TypeScript" (prettier))))
  (setq format-all-formatters '(("TSX" (prettier)))))

;;
;; `haskell-mode`
;;

(use-package haskell-mode
  :ensure true)

;;
;; `fsharp-mode`
;;

(use-package fsharp-mode
  :defer t
  :ensure t
  :config
  (setq fsharp-indent-offset 2)
  (defun fsharp-indent-buffer ()
    "Indent the entire buffer using fsharp-mode indentation."
    (interactive)
    (save-excursion
      (mark-whole-buffer)
      (call-interactively 'fsharp-indent-region)
      (deactivate-mark)
      ))

  :bind (:map fsharp-mode-map ("M-z" . fsharp-indent-buffer))
  )

;;
;; `ruby-mode`
;;

(use-package ruby-mode
  :ensure true)

;;
;; `terraform-mode`
;;

(use-package terraform-mode
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
  (setq gptel-max-tokens 200000        ;Maximum input tokens (200K)
        gptel-response-length 4096)    ;Maximum output tokens (4K)

  (setq gptel-backend
        (gptel-make-anthropic "sonnet-3.7"
          :stream t
          :key "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"
          :models '(claude-3-7-sonnet-20250219)
          :request-params '(:max_tokens 4096)))

  (setq gptel-model 'claude-3-7-sonnet-20250219)
  )

;;
;; `hyper-modern-ai`
;;

(use-package hyper-modern-ai
  :straight (:type built-in)
  :load-path "lib"
  :after gptel
  :config
  (global-set-key (kbd "C-c c") hyper-modern-ai/keymap))

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
   (javascript
    "https://github.com/tree-sitter/tree-sitter-javascript"
    "master" "src")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx
    "https://github.com/tree-sitter/tree-sitter-typescript"
    "master" "tsx/src")
   (typescript
    "https://github.com/tree-sitter/tree-sitter-typescript"
    "master" "typescript/src")
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

;; Add our function as advice to kill-new
(advice-add 'kill-new :after #'sync-kill-to-tmux-clipboard)

;; If you ever need to disable it:
;; (advice-remove 'kill-new #'sync-kill-to-tmux-clipboard)


;; TODO(b7r6): fix it...
(smart-split)
