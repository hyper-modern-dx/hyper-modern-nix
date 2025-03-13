(deftheme base16-ono-sendai-hyper-modern
  "Base16 theme: Ono-Sendai Hyper Modern (Original).")

(let ((base00 "#101216") ; background
      (base01 "#171C22") ; hl-line
      (base02 "#171C22") ; modeline
      (base04 "#262C32")
      (base04 "#262C32")
      (base05 "#96cffe")
      (base06 "#E5EBF2")
      (base07 "#F0F6FC")
      (base08 "#f78166")
      (base09 "#f49b4f")
      (base0A "#f49b4f")
      (base0B "#539bf5")
      (base0C "#d5b7f4")
      (base0D "#539bf5")
      (base0E "#96cffe")
      (base0F "#8ddb8c"))
  (custom-theme-set-faces
   'base16-ono-sendai-hyper-modern
   `(default       ((t (:foreground ,base05 :background ,base00))))
   `(font-lock-comment-face ((t (:foreground ,base05))))
   `(font-lock-string-face  ((t (:foreground ,base05))))
   `(font-lock-keyword-face ((t (:foreground ,base0D))))
   `(font-lock-type-face    ((t (:foreground ,base0A))))
   `(font-lock-builtin-face ((t (:foreground ,base0C))))
   `(region        ((t (:background ,base01))))
   ;; Add additional face customizations as needed.
   ))

(provide-theme 'base16-ono-sendai-hyper-modern)
