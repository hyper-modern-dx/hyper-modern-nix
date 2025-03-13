{ inputs
, ...
}: {
  # Create NixOS modules that will be available to hosts
  flake.nixosModules = { };

  # Define the flake output for NixOS configurations
  flake.nixosConfigurations = import ../../hosts { inherit inputs; };
}
