;;; ono-sendai-hyper-modern-theme.el --- -*- lexical-binding: t -*-

(require 'autothemer)

(autothemer-deftheme
 ono-sendai-hyper-modern "Ono-Sendai: Cyberspace Interfaces for Elite Hackers"

 ((((class color) (min-colors #xFFFFFF))
   ((class color) (min-colors #xFF)))

  ;; Specify the color palette, color columns correspond to each of the classes above.
  (color-lightest             "#000000" nil)
  (color-darkest              "#ffffff" nil)

  (color-bg                   "#101216" nil)
  (color-gray-one             "#161B22" nil)
  (color-gray-two             "#1B1F23" nil)
  (color-gray-three           "#26272A" nil)
  (color-gray-four            "#5C6370" nil)

  (primary-fg                 "#bababa" nil)

  (bright-red                 "#F44747" nil)

  (color-green-one            "#7EE787" nil)
  (color-green-two            "#238636" nil)
  (color-green-three          "#319530" nil)

  (color-blue-one             "#2F81F7" nil)
  (color-blue-two             "#37A3FF" nil)
  (color-blue-three           "#68BAFF" nil)
  (color-blue-four            "#A5D6FF" nil)

  (color-violet-one           "#D2A8FF" nil)
  (color-violet-two           "#BC7EFF" nil)
  (color-violet-three         "#AB5DFF" nil)
  (color-violet-four          "#905DFF" nil)

  (color-yellow-one           "#FDA656" nil)
  (color-yellow-two           "#C68243" nil)
  (color-yellow-three         "#A86E39" nil)
  )


 ((button (:underline t :weight 'bold :foreground color-blue-three))
  (error  (:foreground color-violet-two))

  (default                         (:background color-bg         :foreground primary-fg))
  (hl-line                         (:background color-gray-one   :foreground nil))
  (mode-line                       (:background color-gray-one   :foreground color-gray-three))
  (mode-line-inactive              (:background color-gray-one   :foreground color-gray-two))
  (region                          (:background color-blue-two   :foreground color-lightest))
  (posframe-border                 (:background color-blue-three :foreground nil))

  (success                         (:background nil              :foreground color-blue-three))
  (match                           (:background nil              :foreground color-yellow-one))

  (company-preview                 (:background color-gray-one   :foreground primary-fg))
  (company-tooltip                 (:background color-bg         :foreground primary-fg))
  (company-tooltip-common          (:background color-gray-one   :foreground color-blue-three))
  (company-tooltip-selection       (:background color-gray-one   :foreground color-lightest))
  (company-scrollbar-thumb         (:background color-bg         :foreground color-blue-three))
  (company-tooltip-scrollbar-track (:background color-gray-one   :foreground color-gray-one))


  (magit-section-highlight         (:background color-gray-one   :foreground nil))
  (magit-header-line               (:background nil              :foreground color-yellow-one))

  ;; (magit-section-heading           (:background nil              :foreground color-yellow-one))
  ;; (magit-section-secondary-heading (:background nil              :foreground color-yellow-two))

  (line-number-current-line        (:background color-gray-one   :foreground primary-fg))
  (isearch                         (:background primary-fg       :foreground color-bg))
  (isearch-fail                    (:background primary-fg       :foreground color-lightest))
  (swiper-match                    (:background primary-fg       :foreground color-bg))
  (highlight                       (:background color-blue-three :foreground color-lightest))
  (diff-added                      (:background color-bg         :foreground color-blue-three))
  (diff-removed                    (:background color-bg         :foreground color-violet-one))
  (lazy-highlight                  (:background color-blue-three :distant-foreground color-lightest))
  (fringe                          (:background color-bg         :foreground nil))
  (region                          (:background color-gray-one   :foreground nil))
  (vertical-border                 (:background nil              :foreground color-gray-one))
  (minibuffer-prompt               (:background nil              :foreground primary-fg :bold t))

  (font-lock-comment-face          (:background nil              :foreground color-violet-one))
  (font-lock-constant-face         (:background nil              :foreground color-violet-one))
  (font-lock-function-name-face    (:background nil              :foreground color-blue-three))
  (font-lock-html-tag-face         (:background nil              :foreground color-blue-three))
  (font-lock-type-face             (:background nil              :foreground color-yellow-one))
  (font-lock-variable-face         (:background nil              :foreground color-yellow-one))
  (font-lock-variable-name-face    (:background nil              :foreground color-yellow-one))
  (font-lock-keyword-face          (:background nil              :foreground color-blue-two))
  (font-lock-string-face           (:background nil              :foreground color-blue-three))
  (trailing-whitespace             (:background color-lightest   :foreground nil))
  (line-number                     (:background nil              :foreground color-gray-one))

  (lusty-directory-face            (:background nil              :foreground color-blue-three))
  (lusty-match-face                (:background nil              :foreground color-yellow-one))
  (lusty-file-face                 (:background nil              :foreground nil))
  (lusty-slash-face                (:background nil              :foreground nil))))


;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ono-sendai-hyper-modern)
