{ config
, inputs
, ...
}: {
  flake.homeConfigurations = {
    "b7r6@watchtower" = inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = import inputs.nixpkgs {
        system = "aarch64-darwin";
        config.allowUnfree = true;
      };

      extraSpecialArgs = { inherit inputs; };

      modules = [
        ../common
      ];
    };
  };
}
