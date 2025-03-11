{
  config,
  lib,
  pkgs,
  ...
}: 

{
  # Import default user configuration
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
  programs.bash.shellAliases = {
    # Personal aliases
    team = "cd ~/src/team";
  };
  
  # Additional packages for this user
  home.packages = with pkgs; [
    # User-specific tools can be added here
  ];
  
  # SSH configuration
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        identityFile = "~/.ssh/id_github";
      };
    };
  };
}