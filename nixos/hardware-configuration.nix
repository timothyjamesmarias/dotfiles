# DO NOT EDIT — this is a placeholder.
# Replace with the output of `nixos-generate-config` on your actual hardware.
#
# On the server, run:
#   nixos-generate-config --show-hardware-config > hardware-configuration.nix
#
# Then copy the result here.

{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # TODO: replace with actual hardware config
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos"; # TODO: actual device
    fsType = "ext4";
  };

  swapDevices = [];
}
