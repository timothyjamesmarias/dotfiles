{ config, pkgs, lib, ... }:

{
  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;

    age = {
      # The age private key must exist on the server at this path before first rebuild.
      # Generate with: age-keygen -o /var/lib/sops-nix/key.txt
      # Add the public key to secrets/.sops.yaml, then encrypt secrets with: sops secrets/secrets.yaml
      keyFile = "/var/lib/sops-nix/key.txt";
      sshKeyPaths = []; # Don't derive age keys from SSH keys
    };

    secrets = {
      "ghcr-token" = {
        owner = "root";
      };

      "cloudflare-api-token" = {
        owner = "root";
      };

      "grafana-admin-password" = {
        owner = "root";
      };

      "postgres-exporter-dsn" = {
        owner = "root";
      };

      "auth-service/jwt-secret" = {
        owner = "root";
      };
    };
  };
}
