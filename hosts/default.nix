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

          extraSpecialArgs = {
            inherit inputs;
            currentSystem = "linux";
            isDarwin = false;
            isLinux = true;
          };

          users.b7r6 = { ... }: {
            imports = [
              ../modules/common/cli
              ../modules/stylix
            ];

            # Basic home configuration for Linux
            home = {
              username = "b7r6";
              homeDirectory = "/home/b7r6";
              stateVersion = "24.11";
            };
          };
        };
      }
    ];

    specialArgs = { inherit inputs; };
  };
}
