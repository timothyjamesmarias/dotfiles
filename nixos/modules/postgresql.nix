{ config, pkgs, lib, ... }:

let
  # Central app database registry.
  # Add one entry per app. Each gets a dedicated database + user.
  appDatabases = [
    { name = "example-api"; user = "example_api"; dbName = "example_api"; }
    # { name = "my-app";     user = "my_app";     dbName = "my_app"; }
  ];

  # Docker bridge subnet — containers connect from this range.
  # Verify with: docker network inspect postgres-net | jq '.[0].IPAM.Config[0].Subnet'
  dockerSubnet = "172.18.0.0/16"; # TODO: verify after network creation
in
{
  # --- PostgreSQL ---

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16;

    settings = {
      # Connection limits — PgBouncer sits in front, so this handles pooled connections
      max_connections = 100;

      # Memory — tune based on available RAM
      # TODO: adjust for your hardware (general rule: shared_buffers = 25% of RAM)
      shared_buffers = "256MB";
      effective_cache_size = "768MB";
      work_mem = "4MB";
      maintenance_work_mem = "128MB";

      # WAL
      wal_buffers = "8MB";

      # Logging
      log_min_duration_statement = 1000; # log queries > 1s
    };

    ensureDatabases = map (app: app.dbName) appDatabases;

    ensureUsers = map (app: {
      name = app.user;
      ensureDBOwnership = true;
    }) appDatabases;

    # Per-app authentication: each user can only connect to its own database
    authentication = lib.mkForce (
      ''
        # Local connections (backups, admin)
        local all      postgres                peer
        local all      all                     peer

        # PgBouncer connects via localhost
        host  all      all      127.0.0.1/32   scram-sha-256

        # Per-app rules — Docker containers connect from the bridge subnet
      ''
      + lib.concatMapStringsSep "\n" (app:
        "host  ${app.dbName}  ${app.user}  ${dockerSubnet}  scram-sha-256"
      ) appDatabases
      + ''

        # Postgres exporter (monitoring)
        host  postgres  postgres_exporter  127.0.0.1/32  scram-sha-256
      ''
    );
  };

  # --- PgBouncer ---

  services.pgbouncer = {
    enable = true;

    settings = {
      pgbouncer = {
        pool_mode = "transaction";
        max_client_conn = 400;
        default_pool_size = 20;
        min_pool_size = 5;

        # Listen on all interfaces so Docker containers can reach it.
        # Firewall restricts external access — only Docker bridge subnet connects here.
        listen_addr = "0.0.0.0";
        listen_port = 6432;

        auth_type = "scram-sha-256";
        auth_file = "/etc/pgbouncer/userlist.txt";
      };

      databases = lib.listToAttrs (map (app: {
        name = app.dbName;
        value = "host=127.0.0.1 port=5432 dbname=${app.dbName}";
      }) appDatabases);
    };
  };

  # PgBouncer auth file — generated from app database list.
  # Passwords must be set manually after first deploy:
  #   sudo -u postgres psql -c "ALTER USER example_api PASSWORD 'secret';"
  # Then add the scram hash to this file, or switch to auth_query.
  environment.etc."pgbouncer/userlist.txt" = {
    text = lib.concatMapStringsSep "\n" (app:
      ''"${app.user}" ""'' # TODO: populate with SCRAM hashes after setting passwords
    ) appDatabases;
    mode = "0640";
  };
}
