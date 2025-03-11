{
  config,
  lib,
  pkgs,
  ...
}: 

{
  # User management module
  # This exports user configurations for use in host configurations

  # TODO: Implement user-to-host mapping to determine which users should be created on which hosts
  # For now, we'll make all users available on all hosts
  
  # Export user configurations
  _module.args.userConfigurations = {
    b7r6 = import ./b7r6;
    maskirov = import ./maskirov;
  };
  
  # System-wide user configuration
  # This will apply to all hosts that import this module
  users.users = {
    b7r6 = {
      isNormalUser = true;
      description = "b7r6";
      extraGroups = [ "networkmanager" "wheel" "docker" ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBjp4zAsIR3EsYW1yIRQpaaXSXgaWwMji22rnstPd4cH b7r6@pm.me"
      ];
    };
    
    maskirov = {
      isNormalUser = true;
      description = "maskirov";
      extraGroups = [ "networkmanager" "wheel" "docker" ];
      # TODO: Add SSH keys for maskirov
      openssh.authorizedKeys.keys = [];
    };
  };
  
  # Enable passwordless sudo for wheel group
  security.sudo.wheelNeedsPassword = false;
}