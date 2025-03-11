{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ 
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "virtio_pci" "virtio_scsi" "usbhid" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-arm64" ];
  boot.extraModulePackages = [ ];

  # File systems configuration
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/128bd4e0-78df-4259-815f-8e541f7d15ae";
    fsType = "ext4";
  };
  
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/2069-DC8B";
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" ];
  };

  # Shared folder mount for UTM/QEMU
  # This will mount the QEMU shared folder to /mnt/shared
  fileSystems."/mnt/shared" = {
    device = "shared";
    fsType = "9p";
    options = [ "trans=virtio" "version=9p2000.L" "msize=104857600" "cache=loose" "noauto" "x-systemd.automount" ];
    # Make the mount point if it doesn't exist
    neededForBoot = false;
  };

  swapDevices = [ ];

  # Networking
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s1.useDHCP = lib.mkDefault true;

  # Platform
  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";

  # Additional QEMU guest settings
  services.qemuGuest.enable = true;
}