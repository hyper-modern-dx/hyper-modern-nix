{ config
, lib
, pkgs
, ...
}:
let
  standardEzaFlags = "--icons -F -H --group-directories-first --git -1 --color=always";
in
{
  # Import default user configuration
  imports = [
    ../default
  ];

  home.username = "b7r6";

  # Auto-detect home directory based on platform
  home.homeDirectory =
    if pkgs.stdenv.isDarwin
    then "/Users/b7r6"
    else "/home/b7r6";

  programs.git = {
    userName = "b7r6";
    userEmail = "b7r6@b7r6.net";
  };

  programs.bash.shellAliases = {
    "l" = "eza ${standardEzaFlags}";
    "la" = "eza ${standardEzaFlags} --long --all";
    "lt" = "eza ${standardEzaFlags} --tree --level 2";
    "ltt" = "eza ${standardEzaFlags} --tree --level 3";
    "lttt" = "eza ${standardEzaFlags} --tree --level 4";
    "ltx" = "eza ${standardEzaFlags} --tree --level 99";
  };

  home.packages = with pkgs; [
  ];

  programs.ssh = {
    enable = true;
  };
}
