{ config, pkgs, lib, ... }:

let
  # Docker networks shared across the stack
  networks = [
    "proxy-net"      # Traefik <-> app containers
    "postgres-net"   # App containers <-> PostgreSQL/PgBouncer
    "monitoring-net" # Monitoring stack internal communication
  ];

  # Generate a systemd oneshot service that creates a Docker network if it doesn't exist
  mkNetworkService = name: {
    "docker-network-${name}" = {
      description = "Create Docker network ${name}";
      after = [ "docker.service" ];
      requires = [ "docker.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.docker}/bin/docker network create ${name} || true";
        ExecStop = "${pkgs.docker}/bin/docker network rm ${name} || true";
      };
    };
  };
in
{
  virtualisation.docker = {
    enable = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
      flags = [ "--all" "--volumes" ];
    };
  };

  # Create shared Docker networks
  systemd.services = lib.mkMerge (map mkNetworkService networks ++ [
    {
      # Login to GHCR on boot using sops-decrypted token
      docker-ghcr-login = {
        description = "Login to GitHub Container Registry";
        after = [ "docker.service" "sops-nix.service" ];
        requires = [ "docker.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          # TODO: replace USERNAME with your GitHub username
          ExecStart = pkgs.writeShellScript "ghcr-login" ''
            ${pkgs.docker}/bin/docker login ghcr.io \
              -u USERNAME \
              --password-stdin < ${config.sops.secrets."ghcr-token".path}
          '';
        };
      };
    }
  ]);
}
