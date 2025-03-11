{ config
, inputs
, ...
}: {
  flake.homeConfigurations = {
    # Watchtower - Linux configuration with correct architecture
    "b7r6@watchtower" = inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = import inputs.nixpkgs {
        system = "aarch64-linux"; # Changed to match your actual system
        config.allowUnfree = true;
      };

      extraSpecialArgs = {
        inherit inputs;
        currentSystem = "linux";
      };

      modules = [
        # Core modules
        ../common
        ../stylix
        inputs.stylix.homeManagerModules.stylix

        # Home Manager configuration
        {
          _module.args = {
            isDarwin = false;
            isLinux = true;
          };

          # Basic home configuration for Linux
          home = {
            username = "b7r6";
            homeDirectory = "/home/b7r6";
            stateVersion = "24.11";
          };
        }
      ];
    };
  };
}
