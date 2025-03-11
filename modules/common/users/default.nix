{
  config,
  lib,
  pkgs,
  ...
}: 

{
  # User management module for home-manager configurations
  # This provides configurations for Home Manager to use
  
  # TODO: Implement user-to-host mapping to determine which users should be created on which hosts
  # For now, we'll make all users available on all hosts
  
  # Export user configurations
  _module.args.userConfigurations = {
    b7r6 = import ./b7r6;
    maskirov = import ./maskirov;
  };
}