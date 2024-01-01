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

  -- font = wezterm.font("Liga SFMono Nerd Font", {weight="Medium"}),
  font = wezterm.font("Berkeley Mono", {weight="Bold"}),
  font_size = 14.0,

  default_cursor_style = "BlinkingBlock",
  -- default_cursor_blink_rate = 25,

  -- TODO(armitage): right now these need to be commented out for the HHKB
  -- and uncommented for the laptop keys. armitage-unfsck-this...
  --
  -- send_composed_key_when_left_alt_is_pressed = true,
  -- send_composed_key_when_right_alt_is_pressed = false,
  -- use_ime = false,
  -- use_dead_keys = false,

  -- color_scheme = "GitHub Dark",
  color_scheme = "razorgirl",
}
