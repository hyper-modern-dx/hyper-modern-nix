{ inputs, ... }:
{
  # aarch64-linux hosts
  watchtower = inputs.nixpkgs.lib.nixosSystem {
    system = "aarch64-linux";
    modules = [
      ./nixos-common.nix
      ./watchtower/configuration.nix
      ../modules/stylix

      # External modules
      inputs.home-manager.nixosModules.home-manager
      inputs.stylix.nixosModules.stylix

      # Home Manager configuration
      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users = {
            # Apply both user configurations
            b7r6 = { ... }: { imports = [ ../modules/common/users/b7r6 ]; };
            # maskirov = { ... }: { imports = [ ../modules/common/users/maskirov ]; };
          };
        };
      }
    ];

    specialArgs = { inherit inputs; };
  };
}
