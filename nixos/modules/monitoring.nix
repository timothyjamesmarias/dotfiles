{ config, pkgs, lib, ... }:

{
  # --- Prometheus configuration ---

  environment.etc."prometheus/prometheus.yml".text = builtins.toJSON {
    global = {
      scrape_interval = "15s";
      evaluation_interval = "15s";
    };

    scrape_configs = [
      {
        job_name = "prometheus";
        static_configs = [{ targets = [ "localhost:9090" ]; }];
      }
      {
        job_name = "node-exporter";
        static_configs = [{ targets = [ "node-exporter:9100" ]; }];
      }
      {
        job_name = "postgres-exporter";
        static_configs = [{ targets = [ "postgres-exporter:9187" ]; }];
      }
      {
        job_name = "traefik";
        static_configs = [{ targets = [ "traefik:8080" ]; }];
      }
      {
        job_name = "otel-collector";
        static_configs = [{ targets = [ "otel-collector:8888" ]; }];
      }
    ];
  };

  # --- OTel Collector configuration ---

  environment.etc."otel-collector/config.yaml".text = builtins.toJSON {
    receivers = {
      otlp = {
        protocols = {
          grpc.endpoint = "0.0.0.0:4317";
          http.endpoint = "0.0.0.0:4318";
        };
      };
    };

    processors = {
      batch = {
        timeout = "5s";
        send_batch_size = 1024;
      };
    };

    exporters = {
      prometheus = {
        endpoint = "0.0.0.0:8889";
        namespace = "otel";
      };
      loki = {
        endpoint = "http://loki:3100/loki/api/v1/push";
      };
    };

    service = {
      pipelines = {
        metrics = {
          receivers = [ "otlp" ];
          processors = [ "batch" ];
          exporters = [ "prometheus" ];
        };
        logs = {
          receivers = [ "otlp" ];
          processors = [ "batch" ];
          exporters = [ "loki" ];
        };
      };
    };
  };

  # --- Loki configuration ---

  environment.etc."loki/config.yaml".text = builtins.toJSON {
    auth_enabled = false;

    server.http_listen_port = 3100;

    common = {
      path_prefix = "/loki";
      storage.filesystem.chunks_directory = "/loki/chunks";
      storage.filesystem.rules_directory = "/loki/rules";
      replication_factor = 1;
      ring.kvstore.store = "inmemory";
    };

    schema_config.configs = [{
      from = "2024-01-01";
      store = "tsdb";
      object_store = "filesystem";
      schema = "v13";
      index = {
        prefix = "index_";
        period = "24h";
      };
    }];
  };

  # --- Grafana provisioning ---

  environment.etc."grafana/provisioning/datasources/datasources.yaml".text = builtins.toJSON {
    apiVersion = 1;
    datasources = [
      {
        name = "Prometheus";
        type = "prometheus";
        access = "proxy";
        url = "http://prometheus:9090";
        isDefault = true;
      }
      {
        name = "Loki";
        type = "loki";
        access = "proxy";
        url = "http://loki:3100";
      }
    ];
  };

  # --- Containers ---

  virtualisation.oci-containers.containers = {
    prometheus = {
      image = "prom/prometheus:v3.2.1"; # TODO: pin SHA
      volumes = [
        "/etc/prometheus/prometheus.yml:/etc/prometheus/prometheus.yml:ro"
        "prometheus-data:/prometheus"
      ];
      cmd = [
        "--config.file=/etc/prometheus/prometheus.yml"
        "--storage.tsdb.retention.time=30d"
      ];
      extraOptions = [ "--network=monitoring-net" ];
    };

    grafana = {
      image = "grafana/grafana:11.5.2"; # TODO: pin SHA
      volumes = [
        "grafana-data:/var/lib/grafana"
        "/etc/grafana/provisioning:/etc/grafana/provisioning:ro"
      ];
      environment = {
        GF_SECURITY_ADMIN_USER = "admin";
        # GF_SECURITY_ADMIN_PASSWORD set via environmentFiles
        GF_SERVER_ROOT_URL = "https://grafana.example.com"; # TODO: your domain
      };
      extraOptions = [
        "--network=monitoring-net"
        "--network=proxy-net"
      ];
      labels = {
        "traefik.enable" = "true";
        "traefik.http.routers.grafana.rule" = "Host(`grafana.example.com`)"; # TODO: your domain
        "traefik.http.routers.grafana.entrypoints" = "websecure";
        "traefik.http.routers.grafana.tls.certresolver" = "letsencrypt";
        "traefik.http.routers.grafana.middlewares" = "secure-headers@file";
        "traefik.http.services.grafana.loadbalancer.server.port" = "3000";
      };
    };

    loki = {
      image = "grafana/loki:3.4.2"; # TODO: pin SHA
      volumes = [
        "/etc/loki/config.yaml:/etc/loki/config.yaml:ro"
        "loki-data:/loki"
      ];
      cmd = [ "-config.file=/etc/loki/config.yaml" ];
      extraOptions = [ "--network=monitoring-net" ];
    };

    otel-collector = {
      image = "otel/opentelemetry-collector-contrib:0.120.0"; # TODO: pin SHA
      volumes = [
        "/etc/otel-collector/config.yaml:/etc/otelcol-contrib/config.yaml:ro"
      ];
      extraOptions = [
        "--network=monitoring-net"
        "--network=proxy-net" # Apps on proxy-net send OTLP here
      ];
    };

    node-exporter = {
      image = "prom/node-exporter:v1.9.0"; # TODO: pin SHA
      volumes = [
        "/proc:/host/proc:ro"
        "/sys:/host/sys:ro"
        "/:/rootfs:ro"
      ];
      cmd = [
        "--path.procfs=/host/proc"
        "--path.sysfs=/host/sys"
        "--path.rootfs=/rootfs"
        "--collector.filesystem.mount-points-exclude=^/(sys|proc|dev|host|etc)($$|/)"
      ];
      extraOptions = [
        "--network=monitoring-net"
        "--pid=host"
      ];
    };

    postgres-exporter = {
      image = "prometheuscommunity/postgres-exporter:v0.16.0"; # TODO: pin SHA
      environment = {
        # TODO: set DATA_SOURCE_NAME via environmentFiles from sops secret
        DATA_SOURCE_NAME = "postgresql://postgres_exporter:changeme@host.docker.internal:5432/postgres?sslmode=disable";
      };
      extraOptions = [
        "--network=monitoring-net"
        "--add-host=host.docker.internal:host-gateway"
      ];
    };
  };

  # Ensure monitoring containers start after networks exist
  systemd.services = {
    docker-prometheus.after = [ "docker-network-monitoring-net.service" ];
    docker-prometheus.requires = [ "docker-network-monitoring-net.service" ];

    docker-grafana.after = [ "docker-network-monitoring-net.service" "docker-network-proxy-net.service" ];
    docker-grafana.requires = [ "docker-network-monitoring-net.service" "docker-network-proxy-net.service" ];

    # Connect Grafana to proxy-net after creation (oci-containers only supports one --network)
    docker-grafana.postStart = "${pkgs.docker}/bin/docker network connect proxy-net grafana 2>/dev/null || true";

    docker-loki.after = [ "docker-network-monitoring-net.service" ];
    docker-loki.requires = [ "docker-network-monitoring-net.service" ];

    docker-otel-collector.after = [ "docker-network-monitoring-net.service" "docker-network-proxy-net.service" ];
    docker-otel-collector.requires = [ "docker-network-monitoring-net.service" "docker-network-proxy-net.service" ];
    docker-otel-collector.postStart = "${pkgs.docker}/bin/docker network connect proxy-net otel-collector 2>/dev/null || true";

    docker-node-exporter.after = [ "docker-network-monitoring-net.service" ];
    docker-node-exporter.requires = [ "docker-network-monitoring-net.service" ];

    docker-postgres-exporter.after = [ "docker-network-monitoring-net.service" ];
    docker-postgres-exporter.requires = [ "docker-network-monitoring-net.service" ];
  };
}
