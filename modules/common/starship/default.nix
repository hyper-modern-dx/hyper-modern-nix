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
        style = "${colors.base0B}";
        ssh_only = false;
        disabled = false;
      };

      git_branch = {
        format = "[$branch]($style) ";
        style = "fg:${colors.base0A}";
      };

      directory = {
        format = "[$path]($style) ";
        style = "${colors.base0D}";
        truncate_to_repo = true;
      };

      character = {
        format = "[//](fg:${colors.base03}) ";
        success_symbol = "[//](fg:${colors.base03})";
        error_symbol = "[//](fg:${colors.base0D})";
        disabled = false;
      };

      # Global settings
      scan_timeout = 10;
      add_newline = false;
    };
  };
}
