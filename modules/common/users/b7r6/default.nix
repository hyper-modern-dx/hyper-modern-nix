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

  programs.git = {
    userName = "b7r6";
    userEmail = "b7r6@b7r6.net";
  };

  programs.bash.shellAliases = {
    "z" = "zoxide";
  };

  home.packages = with pkgs; [
  ];

  programs.ssh = {
    enable = true;
  };
}
