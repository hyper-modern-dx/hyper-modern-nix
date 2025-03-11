{ config, lib, pkgs, ... }:
{
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

  # Enable passwordless sudo for wheel group
  security.sudo.wheelNeedsPassword = false;

  # Common system packages
  environment.systemPackages = with pkgs; [
    # Core CLI utilities
    curl
    wget
    git
    vim
    htop
    tmux
  ];
}
