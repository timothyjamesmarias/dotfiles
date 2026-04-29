{ config, pkgs, lib, ... }:

let
  appName  = "example-api";
  domain   = "api.example.com"; # TODO: your domain
  imageSha = "sha256:0000000000000000000000000000000000000000000000000000000000000000"; # TODO: real image digest
  ghOrg    = "USERNAME"; # TODO: your GitHub org or username
  appPort  = "8080";
in
{
  virtualisation.oci-containers.containers.${appName} = {
    image = "ghcr.io/${ghOrg}/${appName}@${imageSha}";

    environment = {
      DATABASE_URL  = "jdbc:postgresql://host.docker.internal:6432/example_api";
      DATABASE_USER = "example_api";
      OTEL_EXPORTER_OTLP_ENDPOINT = "http://otel-collector:4317";
      OTEL_SERVICE_NAME = appName;
    };

    extraOptions = [
      "--network=proxy-net"
      "--add-host=host.docker.internal:host-gateway"
    ];

    labels = {
      "traefik.enable" = "true";
      "traefik.http.routers.${appName}.rule" = "Host(`${domain}`)";
      "traefik.http.routers.${appName}.entrypoints" = "websecure";
      "traefik.http.routers.${appName}.tls.certresolver" = "letsencrypt";
      "traefik.http.routers.${appName}.middlewares" = "forward-auth@file,rate-limit@file,secure-headers@file";
      "traefik.http.services.${appName}.loadbalancer.server.port" = appPort;
    };

    dependsOn = [ "traefik" ];
  };

  systemd.services."docker-${appName}" = {
    after = [ "docker-network-proxy-net.service" ];
    requires = [ "docker-network-proxy-net.service" ];
  };
}
