{ config
, lib
, pkgs
, ...
}:
{
  # Default user configuration that applies to all users
  # Individual users can override these defaults in their specific config

  # Import CLI tool configurations
  imports = [
    ../../cli
  ];

  # Home Manager default configuration
  programs.home-manager.enable = true;

  # Default state version
  # This should be kept at the release version when first installed
  home.stateVersion = "24.11";

  # Default terminal configuration
  programs.bash.initExtra = lib.mkAfter ''
    # Common prompt style used by all users
    # This can be overridden in user-specific configs
    PS1='\[\033[1;36m\]\u\[\033[1;37m\]@\[\033[1;32m\]\h\[\033[1;37m\]:\[\033[1;34m\]\w\[\033[0m\]\$ '
    
    # Add local bin to PATH
    export PATH="$HOME/.local/bin:$PATH"
  '';
}
