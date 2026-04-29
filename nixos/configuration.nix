{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix

    # Base system
    ./modules/base.nix
    ./modules/ssh.nix
    ./modules/firewall.nix
    ./modules/hardening.nix
    ./modules/secrets.nix

    # Infrastructure
    ./modules/docker.nix
    ./modules/postgresql.nix
    ./modules/traefik.nix

    # Observability
    ./modules/monitoring.nix
    ./modules/backups.nix

    # Apps — add new app imports here
    ./apps/example-api.nix
  ];

  networking.hostName = "server"; # TODO: pick a hostname

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "25.05"; # TODO: match your NixOS install version
}
