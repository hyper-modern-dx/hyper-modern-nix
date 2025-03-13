{ inputs, ... }:
{
  watchtower = inputs.nixpkgs.lib.nixosSystem {
    specialArgs = { inherit inputs; };
    system = "aarch64-linux";

    modules = [
      ../modules/stylix
      ./nixos-common.nix
      ./watchtower/configuration.nix
      inputs.home-manager.nixosModules.home-manager
      inputs.stylix.nixosModules.stylix

      # TODO[b7r6]: do this properly...
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

            home = {
              username = "b7r6";
              homeDirectory = "/home/b7r6";
              stateVersion = "24.11";
            };
          };
        };
      }
    ];
  };
}
