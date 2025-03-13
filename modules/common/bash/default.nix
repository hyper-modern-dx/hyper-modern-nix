{ config
, lib
, pkgs
, ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  programs.bash = {
    enable = true;

    shellAliases = {
      update = "sudo nixos-rebuild switch";
      update-flake = "nix flake update && sudo nixos-rebuild switch";
    };

    historyControl = [ "ignoredups" "erasedups" ];
    historyFileSize = 10000;
    historySize = 10000;

    sessionVariables = {
      EDITOR = "nvim";
    };

    initExtra = ''
      export TERM=xterm-256color
      export COLORTERM=truecolor
      export COLORFGBG="15;0"

      # TODO[b7r6]: get more serious about `uv`...
      export PATH="$HOME/.local/bin:$PATH"
    '';
  };
}
