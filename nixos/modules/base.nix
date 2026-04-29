{ config, pkgs, lib, ... }:

{
  # Flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # Locale
  time.timeZone = "America/New_York"; # TODO: your timezone
  i18n.defaultLocale = "en_US.UTF-8";

  # Base packages
  environment.systemPackages = with pkgs; [
    vim
    htop
    curl
    git
    jq
    tmux
    ripgrep
    fd
  ];

  # Deploy user — used for SSH access and managing containers
  users.users.deploy = {
    isNormalUser = true;
    extraGroups = [ "docker" ];
    openssh.authorizedKeys.keys = [
      # TODO: add your SSH public key(s)
      # "ssh-ed25519 AAAA..."
    ];
  };
}
