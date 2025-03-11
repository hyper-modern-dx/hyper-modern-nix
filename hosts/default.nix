{ inputs, ... }:

{
  # aarch64-linux hosts
  watchtower = inputs.nixpkgs.lib.nixosSystem {
    system = "aarch64-linux";
    modules = [
      ./watchtower/configuration.nix
      inputs.home-manager.nixosModules.home-manager
      inputs.stylix.nixosModules.stylix
      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.b7r6 = import ./watchtower/home.nix;
        };
      }
    ];
    specialArgs = { inherit inputs; };
  };

  # Add more hosts here as needed
  # example-x86 = inputs.nixpkgs.lib.nixosSystem { 
  #   system = "x86_64-linux";
  #   ...
  # };
}