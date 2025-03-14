{ config, inputs, ... }:
let
  mkHomeConfig =
    { username
    , hostname
    , system
    , platformType ? if builtins.match ".*darwin" system != null then "darwin" else "linux"
    }:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      extraSpecialArgs = {
        inherit inputs;
        currentSystem = platformType;
      };

      modules = [
        ../common/users/${username}
        ../stylix
        ../wayland
        inputs.stylix.homeManagerModules.stylix
      ];
    };
in
{
  flake.homeConfigurations = {
    "b7r6@watchtower" = mkHomeConfig {
      username = "b7r6";
      hostname = "watchtower";
      system = "aarch64-linux";
    };

    "b7r6@beratna" = mkHomeConfig {
      username = "b7r6";
      hostname = "beratna";
      system = "x86_64-linux";
    };

    "b7r6@galois" = mkHomeConfig {
      username = "b7r6";
      hostname = "galois";
      system = "aarch64-darwin";
    };
  };
}
