{ config
, lib
, pkgs
, ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  programs.wezterm = {
    enable = true;

    extraConfig = ''
      return {
        enable_tab_bar = false,
        enable_title_bar = false,
        default_cursor_style = "BlinkingBlock",
      }
    '';
  };
}
