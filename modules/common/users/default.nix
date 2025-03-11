{ config
, lib
, pkgs
, ...
}:
{
  # User management module for home-manager configurations
  # This provides configurations for Home Manager to use

  # Export user configurations
  _module.args.userConfigurations = {
    b7r6 = import ./b7r6;
    maskirov = import ./maskirov;
  };
}
