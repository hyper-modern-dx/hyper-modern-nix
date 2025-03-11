{
  config,
  inputs,
  ...
}: {
  # Common NixOS module configurations
  # These apply to all NixOS machines
  
  # Import host-specific configurations from the hosts directory
  flake.nixosConfigurations = import ../../hosts { inherit inputs; };
}