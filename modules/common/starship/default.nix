{ config
, lib
, pkgs
, ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  programs.starship = {
    enable = true;
    enableBashIntegration = true;

    settings = {
      format = "[//](fg:${colors.base03})$hostname$git_branch$directory[//](fg:${colors.base03})\n$character";

      hostname = {
        format = " [$hostname]($style) ";
        style = "${colors.base0E}"; # Magenta from base16
        ssh_only = false;
        disabled = false;
      };

      directory = {
        format = "[$path]($style) ";
        style = "${colors.base0D}"; # Blue from base16
        truncate_to_repo = true;
      };

      git_branch = {
        format = "[$branch]($style) ";
        style = "fg:${colors.base09}"; # Orange from base16
      };

      character = {
        format = "[//](fg:${colors.base03}) ";
        success_symbol = "[//](fg:${colors.base03})";
        error_symbol = "[//](fg:${colors.base08})"; # Red from base16
        disabled = false;
      };

      # Global settings
      scan_timeout = 10;
      add_newline = false;
    };
  };
}
