{ config
, pkgs
, inputs
, ...
}:
{
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "watchtower";
    networkmanager.enable = true;
  };

  services.spice-vdagentd.enable = true;

  system.activationScripts = {
    createMountPoints = {
      text = ''
        mkdir -p /mnt/shared
        chown b7r6:users /mnt/shared
        chmod 775 /mnt/shared
      '';
      deps = [ ];
    };
  };

  time.timeZone = "America/Puerto_Rico";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "es_PR.UTF-8";
      LC_IDENTIFICATION = "es_PR.UTF-8";
      LC_MEASUREMENT = "es_PR.UTF-8";
      LC_MONETARY = "es_PR.UTF-8";
      LC_NAME = "es_PR.UTF-8";
      LC_NUMERIC = "es_PR.UTF-8";
      LC_PAPER = "es_PR.UTF-8";
      LC_TELEPHONE = "es_PR.UTF-8";
      LC_TIME = "es_PR.UTF-8";
    };
  };


  virtualisation.docker.enable = true;
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      KbdInteractiveAuthentication = false;
    };
  };

  system.stateVersion = "24.11";
}
