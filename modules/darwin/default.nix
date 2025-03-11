{ config
, inputs
, ...
}: {
  flake.darwinConfigurations = {
    "galois" = inputs.nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        inputs.home-manager.darwinModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.b7r6 = import ../common/users/b7r6;
          };
        }
      ];

      specialArgs = { inherit inputs; };
    };
  };
}
