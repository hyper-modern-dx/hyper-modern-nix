;;; ono-sendai-hyper-modern-theme.el --- -*- lexical-binding: t -*-

(require 'autothemer)

(autothemer-deftheme
 ono-sendai-hyper-modern "Ono-Sendai: Cyberspace Interfaces for Elite Hackers"

 ((((class color) (min-colors #xFFFFFF))
   ((class color) (min-colors #xFF)))

  ;; Specify the color palette, color columns correspond to each of the classes above.
  (color-lightest     "#000000" nil)
  (color-darkest      "#ffffff" nil)

  (color-bg           "#101216" nil)
  (color-gray-one     "#161B22" nil)
  (color-gray-two     "#1B1F23" nil)
  (color-gray-three   "#26272A" nil)
  (color-gray-four    "#5C6370" nil)
  (color-gray-five    "#1B1F23" nil)

  (primary-fg         "#bababa" nil)

  (bright-red         "#F44747" nil)

  (color-green-one    "#7EE787" nil)
  (color-green-two    "#238636" nil)
  (color-green-three  "#319530" nil)

  (color-blue-one     "#2F81F7" nil)
  (color-blue-two     "#37A3FF" nil)
  (color-blue-three   "#68BAFF" nil)
  (color-blue-four    "#A5D6FF" nil)

  (color-violet-one   "#D2A8FF" nil)
  (color-violet-two   "#BC7EFF" nil)
  (color-violet-three "#AB5DFF" nil)
  (color-violet-four  "#905DFF" nil)

  (color-yellow-one   "#FDA656" nil)
  (color-yellow-two   "#C68243" nil)
  (color-yellow-three "#A86E39" nil)

  (color-orange-one   "#FFAB70" nil)

  (color-ansi-black   "#090909" nil)
  (color-ansi-red     "#f78166" nil)
  (color-ansi-green   "#8ddb8c" nil)
  (color-ansi-yellow  "#f49b4f" nil)
  (color-ansi-blue    "#539bf5" nil)
  (color-ansi-magenta "#d5b7f4" nil)
  (color-ansi-cyan    "#96cffe" nil)
  (color-ansi-white   "#ffffff" nil)
  )


 ((button (:underline t :weight 'bold                              :foreground color-blue-three))
  (error  (                                                        :foreground color-violet-two))

  (default                         (:background color-bg           :foreground primary-fg))
  (hl-line                         (:background color-gray-one     :foreground nil))
  (cursor                          (:background color-orange-one   :foreground color-orange-one))
  (mode-line                       (:background color-gray-one     :foreground color-gray-four))
  (mode-line-inactive              (:background color-gray-one     :foreground color-gray-two))
  (region                          (:background color-blue-two     :foreground color-lightest))
  (posframe-border                 (:background color-blue-four    :foreground nil))

  (success                         (:background nil                :foreground color-blue-three))
  (match                           (:background nil                :foreground color-yellow-one))

  (company-preview                 (:background color-gray-one     :foreground primary-fg))
  (company-tooltip                 (:background color-bg           :foreground primary-fg))
  (company-tooltip-common          (:background color-gray-one     :foreground color-blue-three))
  (company-tooltip-selection       (:background color-gray-one     :foreground color-lightest))
  (company-scrollbar-thumb         (:background color-bg           :foreground color-blue-three))
  (company-tooltip-scrollbar-track (:background color-gray-one     :foreground color-gray-one))


  (magit-section-highlight         (:background color-gray-one     :foreground nil))
  (magit-header-line               (:background nil                :foreground color-yellow-one))

  ;; (magit-section-heading           (:background nil             :foreground color-yellow-one))
  ;; (magit-section-secondary-heading (:background nil             :foreground color-yellow-two))

  (line-number-current-line        (:background color-gray-one     :foreground primary-fg))
  (isearch                         (:background primary-fg         :foreground color-bg))
  (isearch-fail                    (:background primary-fg         :foreground color-lightest))
  (swiper-match                    (:background primary-fg         :foreground color-bg))
  (highlight                       (:background color-blue-three   :foreground color-lightest))
  (diff-added                      (:background color-bg           :foreground color-blue-three))
  (diff-removed                    (:background color-bg           :foreground color-violet-one))
  (lazy-highlight                  (:background color-blue-three   :distant-foreground color-lightest))
  (fringe                          (:background color-bg           :foreground nil))
  (region                          (:background color-gray-one     :foreground nil))
  (vertical-border                 (:background nil                :foreground color-gray-one))
  (minibuffer-prompt               (:background nil                :foreground primary-fg :bold t))

  (font-lock-comment-face          (:background nil                :foreground color-violet-one))
  (font-lock-constant-face         (:background nil                :foreground color-violet-one))
  (font-lock-function-name-face    (:background nil                :foreground color-blue-three))
  (font-lock-html-tag-face         (:background nil                :foreground color-blue-three))
  (font-lock-builtin-face          (:background nil                :foreground color-blue-three))
  (font-lock-type-face             (:background nil                :foreground color-yellow-one))
  (font-lock-variable-face         (:background nil                :foreground color-yellow-one))
  (font-lock-variable-name-face    (:background nil                :foreground color-yellow-one))
  (font-lock-keyword-face          (:background nil                :foreground color-blue-two))
  (font-lock-string-face           (:background nil                :foreground color-blue-three))
  (trailing-whitespace             (:background color-lightest     :foreground nil))
  (line-number                     (:background nil                :foreground color-gray-one))

  (lusty-directory-face            (:background nil                :foreground color-blue-three))
  (lusty-match-face                (:background nil                :foreground color-yellow-one))
  (lusty-file-face                 (:background nil                :foreground nil))
  (lusty-slash-face                (:background nil                :foreground nil))

  (ansi-color-black                (:background color-ansi-black   :foreground color-ansi-black))
  (ansi-color-red                  (:background color-ansi-red     :foreground color-ansi-red))
  (ansi-color-green                (:background color-ansi-green   :foreground color-ansi-green))
  (ansi-color-yellow               (:background color-ansi-yellow  :foreground color-ansi-yellow))
  (ansi-color-blue                 (:background color-ansi-blue    :foreground color-ansi-blue))
  (ansi-color-magenta              (:background color-ansi-magenta :foreground color-ansi-magenta))
  (ansi-color-cyan                 (:background color-ansi-cyan    :foreground color-ansi-cyan))
  (ansi-color-white                (:background color-ansi-white   :foreground color-ansi-white))

  (ansi-color-bright-black         (:background color-ansi-black   :foreground color-ansi-black))
  (ansi-color-bright-red           (:background color-ansi-red     :foreground color-ansi-red))
  (ansi-color-bright-green         (:background color-ansi-green   :foreground color-ansi-green))
  (ansi-color-bright-yellow        (:background color-ansi-yellow  :foreground color-ansi-yellow))
  (ansi-color-bright-blue          (:background color-ansi-blue    :foreground color-ansi-blue))
  (ansi-color-bright-magenta       (:background color-ansi-magenta :foreground color-ansi-magenta))
  (ansi-color-bright-cyan          (:background color-ansi-cyan    :foreground color-ansi-cyan))
  (ansi-color-bright-white         (:background color-ansi-white   :foreground color-ansi-white))

  ;; TODO(b7r6): determine if this is still needed with ansi colors defined...
  (term-color-black                (:background color-ansi-black   :foreground color-ansi-black))
  (term-color-red                  (:background color-ansi-red     :foreground color-ansi-red))
  (term-color-green                (:background color-ansi-green   :foreground color-ansi-green))
  (term-color-yellow               (:background color-ansi-yellow  :foreground color-ansi-yellow))
  (term-color-blue                 (:background color-ansi-blue    :foreground color-ansi-blue))
  (term-color-magenta              (:background color-ansi-magenta :foreground color-ansi-magenta))
  (term-color-cyan                 (:background color-ansi-cyan    :foreground color-ansi-cyan))
  (term-color-white                (:background color-ansi-white   :foreground color-ansi-white))

  (vterm-color-black               (:background color-ansi-black   :foreground color-ansi-black))
  (vterm-color-red                 (:background color-ansi-red     :foreground color-ansi-red))
  (vterm-color-green               (:background color-ansi-green   :foreground color-ansi-green))
  (vterm-color-yellow              (:background color-ansi-yellow  :foreground color-ansi-yellow))
  (vterm-color-blue                (:background color-ansi-blue    :foreground color-ansi-blue))
  (vterm-color-magenta             (:background color-ansi-magenta :foreground color-ansi-magenta))
  (vterm-color-cyan                (:background color-ansi-cyan    :foreground color-ansi-cyan))
  (vterm-color-white               (:background color-ansi-white   :foreground color-ansi-white))
  ))



;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ono-sendai-hyper-modern)
