{ config
, lib
, pkgs
, ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  programs.ghostty = {
    enable = true;
  };
}
