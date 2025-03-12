{ config
, lib
, pkgs
, ...
}:

{
  imports = [
    ../default
  ];

  # User identity
  home.username = "maskirov";
  home.homeDirectory = "/home/maskirov";

  # Git identity
  programs.git = {
    userName = "maskirov";
    userEmail = "maskirov@xz.team";
    signing = {
      key = null; # TODO: Add proper GPG key ID
      signByDefault = true;
    };
  };

  # User-specific shell aliases
  programs.bash.shellAliases = { };

  # Additional packages for this user
  home.packages = with pkgs; [
  ];

  programs.ssh = {
    enable = true;
    # matchBlocks = {
    #   "github.com" = {
    #     identityFile = "~/.ssh/id_github";
    #   };
    # };
  };
}
