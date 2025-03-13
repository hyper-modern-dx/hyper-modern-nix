{ config, lib, pkgs, ... }:
{
  nix.settings.experimental-features = [ "flakes" "nix-command" ];
  nixpkgs.config.allowUnfree = true;

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
  ];

  security.sudo.wheelNeedsPassword = false;
  services.tailscale.enable = true;

  users.users = {
    b7r6 = {
      isNormalUser = true;
      description = "b7r6";
      extraGroups = [ "networkmanager" "wheel" "docker" ];

      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBjp4zAsIR3EsYW1yIRQpaaXSXgaWwMji22rnstPd4cH b7r6@pm.me"
      ];
    };

    maskirov = {
      isNormalUser = true;
      description = "maskirov";
      extraGroups = [ "networkmanager" "wheel" "docker" ];

      # TODO: Add SSH keys for maskirov
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBjp4zAsIR3EsYW1yIRQpaaXSXgaWwMji22rnstPd4cH b7r6@pm.me"
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    btop
    cacert
    curl
    git
    home-manager
    htop
    iotop
    spice-vdagent
    tmux
    vim
    wget
    xdg-user-dirs
  ];
}
