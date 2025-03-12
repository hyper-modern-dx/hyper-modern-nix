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
;; Memory Management
;; ============================================================
(defvar hyper-modern/gc-cons-threshold (* 256 1024 1024))

(setq gc-cons-threshold hyper-modern/gc-cons-threshold
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

;; ============================================================
;; Basic UI improvements
;; ============================================================
(menu-bar-mode -1)
(column-number-mode 1)
(global-hl-line-mode 1)
(setq inhibit-startup-screen t)

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

;; Set vertical border to use a continuous line (no dashes)
(when (boundp 'standard-display-table)
  ;; Create the display table if it doesn't exist
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))

  ;; Set the vertical border character
  (set-display-table-slot standard-display-table 'vertical-border ?│))

(setq-default visible-bell nil)
(setq-default ring-bell-function #'ignore)

;; ============================================================
;; Disable all bold and italics (brutally and completely)
;; ============================================================

;; Method 1: Override all faces by remapping font weights
(defun remove-bold-italic-from-all-faces ()
  "Remove all bold and italic attributes from all faces."
  (mapc (lambda (face)
          (when (face-attribute face :weight nil t)
            (set-face-attribute face nil :weight 'normal))
          (when (face-attribute face :slant nil t)
            (set-face-attribute face nil :slant 'normal)))
        (face-list)))

;; Apply once at startup
(remove-bold-italic-from-all-faces)

;; ============================================================
;; Themes and visual enhancements
;; ============================================================

;; Gracefully handle nerd-icons (try to load but don't fail if not available)
(condition-case nil
    (progn
      (require 'nerd-icons)
      (setq doom-modeline-icon t)
      (setq doom-modeline-major-mode-icon t))
  (error
   (message "Nerd icons not available, disabling icons in modeline")
   (setq doom-modeline-icon nil)))

;; Base16 theme configuration
(require 'base16-theme)

;; ============================================================
;; Modern completion framework
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

;; Set up orderless for flexible completion
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; ============================================================
;; Project management
;; ============================================================
(require 'projectile)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; RipGrep configuration
(require 'rg)
(setq rg-group-result t)
(setq rg-context-line-count 2)
(setq rg-show-columns t)
(setq rg-command-line-flags '("--type=all"))

;; Wgrep for editing grep results
(require 'wgrep)
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

;; ============================================================
;; Window management functions
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
;; Modeline configuration
;; ============================================================

(require 'doom-modeline)
(doom-modeline-mode 1)

;; ============================================================
;; Programming mode enhancements
;; ============================================================

;; Rainbow delimiters in programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Parenthesis handling
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(require 'paredit-everywhere)
(add-hook 'prog-mode-hook #'paredit-everywhere-mode)

;; Direnv integration
(require 'direnv)
(direnv-mode 1)

;; Nicer org mode bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ============================================================
;; Better defaults
;; ============================================================

(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Terminal-specific settings
(xterm-mouse-mode 1)  ;; Enable mouse in terminal

;; ============================================================
;; Tree-sitter Configuration
;; ============================================================
(require 'treesit-auto)

;; Define the language sources
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
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")
   (c "https://github.com/tree-sitter/tree-sitter-c")
   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
   (java "https://github.com/tree-sitter/tree-sitter-java")
   (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
   (nix "https://github.com/nix-community/tree-sitter-nix")
   (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

;; Function to install all grammars
(defun ensure-treesit-languages ()
  "Ensure all languages in treesit-language-source-alist are installed."
  (dolist (lang-source treesit-language-source-alist)
    (let ((lang (car lang-source)))
      (unless (treesit-language-available-p lang)
        (message "Installing tree-sitter grammar for %s" lang)
        (treesit-install-language-grammar lang)))))

;; Install grammars when tree-sitter is available
(when (treesit-available-p)
  (ensure-treesit-languages))

;; Enable treesit-auto mode
(global-treesit-auto-mode)

;; ============================================================
;; Format-all configuration
;; ============================================================
(require 'format-all)

;; Set up default formatters for different languages
(setq format-all-default-formatters
      '(("Nix" nixpkgs-fmt)
        ("Python" (ruff-format))
        ("TypeScript" prettier)
        ("TSX" prettier)
        ("JavaScript" prettier)
        ("JSON" prettier)
        ("HTML" prettier)
        ("CSS" prettier)
        ("YAML" prettier)
        ("Markdown" prettier)
        ("C" clang-format)
        ("C++" clang-format)
        ("Java" clang-format)
        ("Rust" rustfmt)
        ("Swift" swift-format)))

;; Add format-all to prog-mode
(add-hook 'prog-mode-hook 'format-all-mode)

;; ============================================================
;; Language-specific configurations
;; ============================================================

;; --- Nix ---
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(with-eval-after-load 'nix-mode
  (define-key nix-mode-map (kbd "M-z") 'nixpkgs-fmt-buffer))

;; --- Python ---
(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "M-z")
              (lambda ()
                (interactive)
                (ruff-format-buffer))))

;; --- JavaScript / TypeScript ---
(require 'typescript-ts-mode)

;; Use tree-sitter modes for JS/TS
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-ts-mode))

;; Indent settings
(setq typescript-indent-level 2)
(setq js-indent-level 2)

;; Formatter binding
(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-mode-map (kbd "M-z")
              (lambda ()
                (interactive)
                (prettier-format-buffer))))

;; --- C/C++ ---
(require 'cc-mode)
(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "M-z") 'clang-format-buffer)
  (define-key c++-mode-map (kbd "M-z") 'clang-format-buffer))

;; --- Java ---
(when (fboundp 'java-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
  (with-eval-after-load 'java-ts-mode
    (define-key java-ts-mode-map (kbd "M-z") 'clang-format-buffer)))

;; --- JSON ---
(when (fboundp 'json-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (with-eval-after-load 'json-ts-mode
    (define-key json-ts-mode-map (kbd "M-z")
                (lambda ()
                  (interactive)
                  (prettier-format-buffer)))))

;; --- YAML ---
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(with-eval-after-load 'yaml-mode
  (define-key yaml-mode-map (kbd "M-z")
              (lambda ()
                (interactive)
                (prettier-format-buffer))))

;; --- Markdown ---
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "M-z")
              (lambda ()
                (interactive)
                (prettier-format-buffer))))

;; --- Lua ---
(require 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;; --- Haskell ---
(require 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

;; --- Rust ---
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "M-z") 'rustfmt-format-buffer))

;; --- Swift ---
(require 'swift-mode)
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))
(with-eval-after-load 'swift-mode
  (define-key swift-mode-map (kbd "M-z") 'swift-format-buffer))

;; --- F# ---
(require 'fsharp-mode)
(add-to-list 'auto-mode-alist '("\\.fs[iylx]?\\'" . fsharp-mode))
(with-eval-after-load 'fsharp-mode
  (define-key fsharp-mode-map (kbd "M-z")
              (lambda ()
                (interactive)
                (save-excursion
                  (mark-whole-buffer)
                  (call-interactively 'fsharp-indent-region)
                  (deactivate-mark)))))

;; --- Terraform ---
(require 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))

;; --- Docker ---
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; --- Zig ---
(require 'zig-mode)
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

;; --- Just ---
(require 'just-mode)
(add-to-list 'auto-mode-alist '("Justfile\\'" . just-mode))
(add-to-list 'auto-mode-alist '("\\.just\\'" . just-mode))

;; --- Protobuf ---
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; --- Prisma ---
(when (require 'prisma-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.prisma\\'" . prisma-mode)))

;; --- CSV ---
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
             '(python-mode . ("pylsp")))

(add-to-list 'eglot-server-programs
             '((c-mode c++-mode) . ("clangd")))

(add-to-list 'eglot-server-programs
             '(rust-mode . ("rust-analyzer")))

(add-to-list 'eglot-server-programs
             '(java-ts-mode . ("java-language-server")))

(add-to-list 'eglot-server-programs
             '(nix-mode . ("nixd")))

;; Disable flymake
(setq eglot-stay-out-of '("flymake"))

;; Auto-start eglot for programming modes
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

;; Disable inlay hints (they can be distracting)
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (eglot-inlay-hints-mode -1)
            (flymake-mode -1)))

;; ============================================================
;; Company Configuration for Completion
;; ============================================================

(require 'company)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(global-company-mode 1)

;; Bind M-TAB to trigger company completion
(global-set-key (kbd "M-TAB") #'company-complete)

;; ============================================================
;; Magit Configuration
;; ============================================================

(require 'magit)

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
(setq magit-display-buffer-function #'my-magit-display-buffer-function)

;; ============================================================
;; Compilation Mode
;; ============================================================

(require 'compile)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; ============================================================
;; Key bindings
;; ============================================================
(require 'general)

(general-define-key
 ;; Standard movement
 "C-c q"   'join-line
 "C-c r"   'revert-buffer
 "C-j"     'newline-and-indent

 ;; Frame manipulation
 "C-x C-+" 'text-scale-increase
 "C-x C--" 'text-scale-decrease
 "C-x C-r" 'rg-dwim-project-dir
 "C-x d"   'consult-recent-file

 ;; b7r6 standard keys
 "M-/"     'undo
 "M-N"     'windmove-right
 "M-P"     'windmove-left
 "M-z"     'format-all-region-or-buffer  ;; Global fallback for M-z

 ;; hyper-modern overrides
 "C-M-r"   'consult-ripgrep
 "M-R"     'hyper-modern/rotate-windows
 "C-x 2"   'hyper-modern/vsplit
 "C-x 3"   'hyper-modern/hsplit
 "C-x g"   'magit-status
 "C-s"     'consult-line
 "C-c r"   'consult-ripgrep)

;; ============================================================
;; Vterm configuration
;; ============================================================

(defun setup-vterm ()
  (when (fboundp 'vterm-mode)
    (setq vterm-keymap-exceptions '("M-/" "M-N" "M-P" "M-i" "M-z"))
    (with-eval-after-load 'vterm
      (define-key vterm-mode-map (kbd "M-N") 'windmove-right)
      (define-key vterm-mode-map (kbd "M-P") 'windmove-left))))

(add-hook 'vterm-mode-hook (lambda ()
                             (setq-local global-hl-line-mode nil)
                             (setup-vterm)))

;; ============================================================
;; TMUX Clipboard Integration
;; ============================================================

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

;; Enable recentf for recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

;; ============================================================
;; Dashboard Configuration
;; ============================================================

(when (require 'dashboard nil t)
  ;; Create a temporary file with your ASCII art
  (defvar my-custom-banner-file (make-temp-file "emacs-dashboard-banner-" nil ".txt"))

  ;; Your ASCII art banner
  (defvar my-custom-banner-text "
╦ ╦╦ ╦╔═╗╔═╗╦═╗  ╔╦╗╔═╗╔╦╗╔═╗╦═╗╔╗╔
╠═╣╚╦╝╠═╝║╣ ╠╦╝  ║║║║ ║ ║║║╣ ╠╦╝║║║
╩ ╩ ╩ ╩  ╚═╝╩╚═  ╩ ╩╚═╝═╩╝╚═╝╩╚═╝╚╝
")

  ;; Write the banner to the temporary file
  (with-temp-file my-custom-banner-file
    (insert my-custom-banner-text))

  ;; Use our temporary file as the banner
  (setq dashboard-startup-banner my-custom-banner-file)

  ;; Custom title
  (setq dashboard-banner-logo-title
        "it was the style that mattered and the style was the same.
the moderns were mercenaries, practical jokers, nihilistic tehcnofetishists.")

  ;; Other dashboard settings
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((projects . 5)
                          (recents . 5)))

  ;; Start dashboard
  (dashboard-setup-startup-hook))

;; ============================================================
;; GPTel Configuration
;; ============================================================
(when (require 'gptel nil t)
  (setq gptel-model "gpt-4o")
  (setq gptel-max-tokens 4096))

;; ============================================================
;; Dirvish (Better directory viewer)
;; ============================================================
(when (require 'dirvish nil t)
  (dirvish-override-dired-mode))
