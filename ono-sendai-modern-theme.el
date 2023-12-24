(deftheme ono-sendai-modern
  "Emacs Color Theme Based on Modern Trends in Source Code Rendering")

;; Utility Variables
(let* (
       (vibrant-pink "#EF596F")
       (soft-purple "#D55FDE")
       (warm-tan "#D19A66")
       (light-tan "#E5C07B")
       (sky-blue "#61AFEF")
       (fresh-green "#89CA78")
       (dark-gray "#5C6370")
       (aqua-blue "#2BBAC5")
       (pure-white "#FFFFFF")
       (bright-red "#F44747")
       (medium-gray "#7F848E")
       (deep-orange "#BE5046")

       (primary-bg "#171B22")
       (secondary-bg "#101216")
       (primary-fg "#bababa")
       (highlight-fg "#539bf5")
       (alert-fg "#f49b4f")
       (comment-fg "#d5b7f4")
       (constant-fg "#ffffff")
       (soft-gray "#ABB2BF")
       )

  (custom-theme-set-faces
   'ono-sendai-modern

   `(default ((t (:background ,secondary-bg :foreground ,primary-fg))))

   `(mode-line-inactive ((t (:background ,primary-bg :foreground ,secondary-bg :box (:line-width 1 :color ,primary-bg)))))
   `(mode-line ((t (:background ,primary-bg :foreground ,sky-blue :box (:line-width 1 :color ,primary-bg)))))

   `(company-preview ((t (:background ,primary-bg :foreground ,primary-fg))))
   `(company-tooltip ((t (:background ,secondary-bg :foreground ,primary-fg))))
   `(company-tooltip-common ((t (:background ,primary-bg :foreground ,highlight-fg))))
   `(company-tooltip-selection ((t (:background ,primary-bg :foreground ,constant-fg))))
   `(company-scrollbar-thumb ((t (:background ,secondary-bg :foreground ,highlight-fg))))
   `(company-tooltip-scrollbar-track ((t (:background ,primary-bg :foreground ,primary-bg))))

   `(hl-line ((t (:background ,primary-bg))))
   `(fringe ((t (:background ,secondary-bg))))
   `(region ((t (:background ,primary-bg))))
   `(vertical-border ((t (:foreground ,primary-bg))))
   `(minibuffer-prompt ((t (:foreground ,primary-fg :bold t))))

   `(font-lock-comment-face ((t (:foreground ,comment-fg))))
   `(font-lock-constant-face ((t (:foreground ,comment-fg))))
   `(font-lock-function-name-face ((t (:foreground ,highlight-fg))))
   `(font-lock-html-tag-face ((t (:foreground ,highlight-fg))))
   `(font-lock-type-face ((t (:foreground ,alert-fg))))
   `(font-lock-variable-face ((t (:foreground ,alert-fg))))
   `(font-lock-variable-name-face ((t (:foreground ,alert-fg))))
   `(font-lock-keyword-face ((t (:foreground ,highlight-fg))))
   `(font-lock-string-face ((t (:foreground ,highlight-fg))))
   `(trailing-whitespace ((t (:background ,constant-fg))))

   `(line-number ((t (:foreground ,primary-bg))))
   `(line-number-current-line ((t (:foreground ,primary-fg :background ,primary-bg))))

   `(isearch ((t (:foreground ,secondary-bg :background ,primary-fg))))
   `(isearch-fail ((t (:foreground ,constant-fg :background ,primary-fg))))
   `(swiper-match ((t (:foreground ,secondary-bg :background ,primary-fg))))
   `(highlight ((t (:background ,highlight-fg :foreground "white"))))
   `(lazy-highlight ((t (:background ,highlight-fg :distant-foreground "white"))))
   `(diff-added ((t (:foreground ,highlight-fg :background ,secondary-bg))))
   `(diff-removed ((t (:foreground ,comment-fg :background ,secondary-bg))))

   `(lusty-directory-face ((t (:foreground ,sky-blue))))
   `(lusty-file-face ((t nil)))
   `(lusty-match-face ((t (:foreground ,alert-fg))))
   `(lusty-slash-face ((t nil)))
   ;; Additional faces...
   ))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ono-sendai-modern)

;; (deftheme ono-sendai-modern
;;   "Emacs Color Theme Based on Modern Trends in Source Code Rendering")

;; (custom-theme-set-faces
;;  'ono-sendai-modern
;;  '(mode-line ((t (:background "#171B22" :foreground "#96cffe" :box (:line-width 1 :color "#171B22")))))
;;  '(mode-line-inactive ((t (:background "#171B22" :foreground "#96cffe" :box (:line-width 1 :color "#171B22")))))
;;  '(company-preview ((t (:background "#171B22" :foreground "#bababa"))))
;;  '(company-tooltip ((t (:background "#101216" :foreground "#bababa"))))
;;  '(company-tooltip-common ((t (:background "#171B22" :foreground "#539bf5"))))
;;  '(company-tooltip-selection ((t (:background "#171B22" :foreground "#ffffff"))))
;;  '(company-scrollbar-thumb ((t (:background "#101216" :foreground "#539bf5"))))
;;  '(company-tooltip-scrollbar-track ((t (:background "#171B22" :foreground "#171B22"))))
;;  '(company-tooltip-selection ((t (:background "#171B22" :foreground "#f49b4f"))))
;;  '(hl-line ((t (:background "#171B22"))))
;;  '(fringe ((t (:background "#101216"))))
;;  '(region ((t (:background "#171B22"))))
;;  '(vertical-border ((t (:foreground "#171B22"))))
;;  '(minibuffer-prompt ((t (:foreground "#bababa" :bold t))))
;;  '(font-lock-comment-face ((t (:foreground "#d5b7f4"))))
;;  '(font-lock-constant-face ((t (:foreground "#d5b7f4"))))
;;  '(font-lock-function-name-face ((t (:foreground "#539bf5"))))
;;  '(font-lock-html-tag-face ((t (:foreground "#539bf5"))))
;;  '(font-lock-type-face ((t (:foreground "#f49b4f"))))
;;  '(font-lock-variable-face ((t (:foreground "#f49b4f"))))
;;  '(font-lock-variable-name-face ((t (:foreground "#f49b4f"))))
;;  '(font-lock-keyword-face ((t (:foreground "#96cffe"))))
;;  '(font-lock-string-face ((t (:foreground "#96cffe"))))
;;  '(font-lock-constant-face ((t (:foreground "#bababa"))))
;;  '(trailing-whitespace ((t (:background "#ffffff"))))
;;  '(line-number ((t (:foreground "#171B22"))))
;;  '(line-number-current-line ((t (:foreground "#bababa" :background "#171B22"))))
;;  '(isearch ((t (:foreground "#101216" :background "#bababa"))))
;;  '(isearch-fail ((t (:foreground "#ffffff" :background "#bababa"))))
;;  '(swiper-match ((t (:foreground "#101216" :background "#bababa"))))
;;  '(highlight ((t (:background "#96cffe" :foreground "white"))))
;;  '(lazy-highlight ((t (:background "#96cffe" :distant-foreground "white"))))
;;  '(diff-added ((t (:foreground "#539bf5" :background "#101216"))))
;;  '(diff-removed ((t (:foreground "#d5b7f4" :background "#101216"))))
;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "#bababa"))))
;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "#d5b7f4"))))
;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "#539bf5"))))
;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "#96cffe"))))
;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "#96cffe"))))
;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "#539bf5"))))
;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "#d5b7f4"))))
;;  '(rainbow-delimiters-depth-8-face ((t (:foreground "#171B22"))))
;;  '(rainbow-delimiters-depth-9-face ((t (:foreground "#ffffff"))))
;;  '(lusty-directory-face ((t (:foreground "#539bf5"))))
;;  '(lusty-file-face ((t nil)))
;;  '(lusty-match-face ((t (:foreground "#96cffe"))))
;;  '(lusty-slash-face ((t nil)))
;;  '(ido-only-match ((t (:foreground "#FF8400"))))
;;  '(ido-subdir ((t (:foreground "#5AA6FF"))))
;;  '(default ((t (:background "#101216" :foreground "#bababa"))))
;;  '(lusty-directory-face ((t (:foreground "cyan"))))
;;  '(lusty-file-face ((t nil)) t)
;;  '(lusty-match-face ((t (:foreground "blue"))))
;;  '(lusty-slash-face ((t nil)))
;;  )

;; (provide-theme 'ono-sendai-modern)
