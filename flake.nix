{
  description = "You Must Always Have a Knife in The Darkness...";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-untils.follows = "flake-utils";
    };
  };

  outputs = { self, ... }@inputs:
    let
      system = "x86_64-linux";
      username = builtins.getEnv "USER";
      homeDirectory = "/home/${username}";

      pkgs = import ./nix/nixpkgs.nix { inherit inputs system; };

      configuration =
        import ./home.nix { inherit pkgs inputs username homeDirectory; };

    in {
      homeConfigurations = {
        "${username}" = inputs.home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
            ./home.nix
            {
              home = {
                inherit username homeDirectory;
                stateVersion = "22.11";
              };
            }
          ];
        };
      };
    };
}
