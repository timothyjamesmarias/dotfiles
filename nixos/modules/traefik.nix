{ config, pkgs, lib, ... }:

{
  # Traefik static configuration
  environment.etc."traefik/traefik.yml".text = builtins.toJSON {
    entryPoints = {
      web = {
        address = ":80";
        http.redirections.entryPoint = {
          to = "websecure";
          scheme = "https";
        };
      };
      websecure = {
        address = ":443";
        http.tls.certResolver = "letsencrypt";
      };
    };

    certificatesResolvers.letsencrypt.acme = {
      email = "you@example.com"; # TODO: your email
      storage = "/acme/acme.json";
      dnsChallenge = {
        provider = "cloudflare";
        delayBeforeCheck = 10;
      };
    };

    providers.docker = {
      endpoint = "unix:///var/run/docker.sock";
      exposedByDefault = false;
      network = "proxy-net";
    };

    # Metrics endpoint for Prometheus
    metrics.prometheus = {
      entryPoint = "websecure";
    };

    log.level = "WARN";
    accessLog = {};
  };

  # Traefik dynamic configuration — middlewares available to all routers
  environment.etc."traefik/dynamic.yml".text = builtins.toJSON {
    http.middlewares = {
      rate-limit = {
        rateLimit = {
          average = 100;
          burst = 200;
          period = "1m";
        };
      };
      secure-headers = {
        headers = {
          stsSeconds = 31536000;
          stsIncludeSubdomains = true;
          stsPreload = true;
          forceSTSHeader = true;
          contentTypeNosniff = true;
          frameDeny = true;
          browserXssFilter = true;
          referrerPolicy = "strict-origin-when-cross-origin";
        };
      };
      # ForwardAuth — delegates authentication to your auth service.
      # Requests are forwarded to the auth service; 2xx = allow, 4xx = deny.
      forward-auth = {
        forwardAuth = {
          address = "http://auth-service:8080/verify"; # TODO: your auth service endpoint
          trustForwardHeader = true;
          authResponseHeaders = [ "X-User-Id" "X-User-Role" ];
        };
      };
    };
  };

  virtualisation.oci-containers.containers.traefik = {
    image = "traefik:v3.3"; # TODO: pin to specific SHA
    ports = [
      "80:80"
      "443:443"
    ];
    volumes = [
      "/var/run/docker.sock:/var/run/docker.sock:ro"
      "/etc/traefik/traefik.yml:/traefik.yml:ro"
      "/etc/traefik/dynamic.yml:/etc/traefik/dynamic.yml:ro"
      "traefik-acme:/acme"
    ];
    environment = {
      CF_API_EMAIL = "you@example.com"; # TODO: your Cloudflare email
    };
    environmentFiles = [
      # Provides CF_DNS_API_TOKEN for Cloudflare DNS challenge
      # Create this file from the sops secret
    ];
    extraOptions = [
      "--network=proxy-net"
    ];
    labels = {
      "traefik.enable" = "false"; # Traefik doesn't route to itself
    };
  };

  # Ensure Traefik starts after networks exist
  systemd.services.docker-traefik = {
    after = [
      "docker-network-proxy-net.service"
    ];
    requires = [
      "docker-network-proxy-net.service"
    ];
  };

  # Write Cloudflare API token to a file Traefik can read
  systemd.services.traefik-env = {
    description = "Generate Traefik environment file from secrets";
    after = [ "sops-nix.service" ];
    before = [ "docker-traefik.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "traefik-env" ''
        mkdir -p /run/traefik
        echo "CF_DNS_API_TOKEN=$(cat ${config.sops.secrets."cloudflare-api-token".path})" > /run/traefik/env
        chmod 600 /run/traefik/env
      '';
    };
  };
}
