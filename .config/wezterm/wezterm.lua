local wezterm = require 'wezterm';

return {
  audible_bell = "Disabled",

  visual_bell = {
    fade_in_duration_ms = 75,
    fade_out_duration_ms = 75,
    target = "CursorColor",
  },

  term = "xterm-16color",

  enable_tab_bar = false,

  -- TODO(b7r6): figure out how to do font fallback for pagans who
  -- lack berkeley mono...

  -- font = wezterm.font("Liga SFMono Nerd Font", {weight="Medium"}),
  -- font = wezterm.font("BerkeleyMono Nerd Font Mono", {weight="Medium"}),

  font = wezterm.font("Berkeley Mono", {weight="Bold"}),
  font_size = 13.5,
  cell_width = 0.95,

  default_cursor_style = "BlinkingBlock",

  -- TODO(b7r6): right now these need to be commented out for the HHKB
  -- and uncommented for the laptop keys. b7r6-unfsck-this...
  --
  -- send_composed_key_when_left_alt_is_pressed = true,
  -- send_composed_key_when_right_alt_is_pressed = false,
  -- use_ime = false,
  -- use_dead_keys = false,

  color_scheme = "ono-sendai-hyper-modern",
  window_decorations = "RESIZE",
}
