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

  # User identity
  home.username = "b7r6";
  home.homeDirectory = "/home/b7r6";

  # Git identity
  programs.git = {
    userName = "b7r6";
    userEmail = "b7r6@b7r6.net";
    # signing = {
    #   key = "6A0E095D8366C287";
    #   signByDefault = true;
    # };
  };

  # User-specific shell aliases
  programs.bash.shellAliases = {
    "z" = "zoxide";
  };

  # Additional packages for this user
  home.packages = with pkgs; [
    # User-specific tools can be added here
  ];

  # SSH configuration
  programs.ssh = {
    enable = true;
    # matchBlocks = {
    #   "github.com" = {
    #     identityFile = "~/.ssh/id_github";
    #   };
    # };
  };
}
