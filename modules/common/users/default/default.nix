{
  config,
  lib,
  pkgs,
  ...
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
  
  # TODO: Implement user-to-host mapping to determine which users should be created on which hosts
  
  # Common user aliases and environment setup
  programs.bash.shellAliases = {
    # Add any shared aliases across all users here
    hosts = "cat /etc/hosts";
    nixr = "cd ~/src/nixos-config-aarch64";
  };
  
  # Default terminal configuration
  programs.bash.initExtra = lib.mkAfter ''
    # Common prompt style used by all users
    # This can be overridden in user-specific configs
    PS1='\[\033[1;36m\]\u\[\033[1;37m\]@\[\033[1;32m\]\h\[\033[1;37m\]:\[\033[1;34m\]\w\[\033[0m\]\$ '
    
    # Add local bin to PATH
    export PATH="$HOME/.local/bin:$PATH"
  '';
}