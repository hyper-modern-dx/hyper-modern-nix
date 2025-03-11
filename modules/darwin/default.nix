{
  config,
  inputs,
  ...
}: {
  # Common Darwin module configurations
  # These apply to all macOS machines
  
  # Import host-specific configurations from the hosts directory
  flake.darwinConfigurations = {
    # Uncomment and modify when adding Darwin hosts
    # "macbook" = inputs.nix-darwin.lib.darwinSystem {
    #   system = "aarch64-darwin";
    #   modules = [
    #     inputs.home-manager.darwinModules.home-manager
    #     {
    #       home-manager = {
    #         useGlobalPkgs = true;
    #         useUserPackages = true;
    #         users.b7r6 = import ../../hosts/macbook/home.nix;
    #       };
    #     }
    #   ];
    #   specialArgs = { inherit inputs; };
    # };
  };
}