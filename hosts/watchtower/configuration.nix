{ config, pkgs, inputs, ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/common # Import common modules including stylix
  ];

  # Create shared directory mount point
  system.activationScripts = {
    createMountPoints = {
      text = ''
        mkdir -p /mnt/shared
        chown b7r6:users /mnt/shared
        chmod 775 /mnt/shared
      '';
      deps = [];
    };
  };

  # Enable flakes support
  nix.settings.experimental-features = ["flakes" "nix-command"];

  # Boot configuration
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Networking
  networking = {
    hostName = "watchtower";
    networkmanager.enable = true;
  };
  
  # Enable SPICE agent for clipboard sharing with UTM/QEMU
  services.spice-vdagentd.enable = true;

  # Tailscale VPN
  services.tailscale.enable = true;

  # Time and locale settings
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

  # User configuration
  users.users.b7r6 = {
    isNormalUser = true;
    description = "b7r6";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBjp4zAsIR3EsYW1yIRQpaaXSXgaWwMji22rnstPd4cH b7r6@pm.me"
    ];
  };

  # Enable passwordless sudo for wheel group
  security.sudo.wheelNeedsPassword = false;

  # Enable automatic login for the user
  services.getty.autologinUser = "b7r6";

  # Package management
  nixpkgs.config.allowUnfree = true;
  
  # Development tools and utilities
  environment.systemPackages = with pkgs; [
    # Core tools
    neovim
    git
    ripgrep
    fd
    curl
    wget
    tmux
    
    # Development tools
    gnumake
    gcc
    jq
    
    # Nix development tools
    nixd  # Modern Nix LSP
    nixpkgs-fmt
    
    # Python tools (Astral)
    python3
    (python3.withPackages (ps: with ps; [
      pip
    ]))
    ruff
    
    # System utilities
    htop
    btop
    iotop
    dnsutils
    lsof
    
    # QEMU/UTM utilities
    spice-vdagent
    xdg-user-dirs  # For managing standard user directories
  ];

  # Docker support
  virtualisation.docker.enable = true;

  # SSH configuration for passwordless login
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      KbdInteractiveAuthentication = false;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  system.stateVersion = "24.11"; # Did you read the comment?
}