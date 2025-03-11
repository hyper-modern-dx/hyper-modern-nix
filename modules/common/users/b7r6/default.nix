{ config
, lib
, pkgs
, ...
}:
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

  programs.bash.shellAliases = { };

  home.packages = with pkgs; [
  ];

  programs.ssh = {
    enable = true;
  };
}
