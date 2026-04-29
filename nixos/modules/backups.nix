{ config, pkgs, lib, ... }:

let
  backupDir = "/var/backups";

  # Databases to back up — keep in sync with postgresql.nix appDatabases
  databases = [ "example_api" ]; # TODO: add database names as you add apps

  pgBackupScript = pkgs.writeShellScript "pg-backup" ''
    set -euo pipefail
    DATE=$(date +%Y-%m-%d_%H%M%S)
    BACKUP_DIR="${backupDir}/postgresql"
    mkdir -p "$BACKUP_DIR"

    for DB in ${lib.concatStringsSep " " databases}; do
      echo "Backing up database: $DB"
      ${pkgs.postgresql_16}/bin/pg_dump -U postgres "$DB" \
        | ${pkgs.gzip}/bin/gzip > "$BACKUP_DIR/$DB-$DATE.sql.gz"
    done

    # Prune backups older than 30 days
    ${pkgs.findutils}/bin/find "$BACKUP_DIR" -name "*.sql.gz" -mtime +30 -delete
    echo "PostgreSQL backup complete"
  '';

  volumeBackupScript = pkgs.writeShellScript "volume-backup" ''
    set -euo pipefail
    DATE=$(date +%Y-%m-%d_%H%M%S)
    BACKUP_DIR="${backupDir}/volumes"
    mkdir -p "$BACKUP_DIR"

    # Back up all named Docker volumes
    for VOL in $(${pkgs.docker}/bin/docker volume ls -q); do
      echo "Backing up volume: $VOL"
      ${pkgs.docker}/bin/docker run --rm \
        -v "$VOL":/source:ro \
        -v "$BACKUP_DIR":/backup \
        alpine tar czf "/backup/$VOL-$DATE.tar.gz" -C /source .
    done

    # Prune volume backups older than 30 days
    ${pkgs.findutils}/bin/find "$BACKUP_DIR" -name "*.tar.gz" -mtime +30 -delete
    echo "Volume backup complete"
  '';
in
{
  # Daily PostgreSQL backup at 3 AM
  systemd.services.pg-backup = {
    description = "PostgreSQL database backup";
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      ExecStart = pgBackupScript;
    };
  };

  systemd.timers.pg-backup = {
    description = "Daily PostgreSQL backup timer";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 03:00:00";
      Persistent = true; # Run missed backups after downtime
    };
  };

  # Weekly Docker volume backup at 4 AM Sunday
  systemd.services.volume-backup = {
    description = "Docker volume backup";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = volumeBackupScript;
    };
  };

  systemd.timers.volume-backup = {
    description = "Weekly Docker volume backup timer";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "Sun *-*-* 04:00:00";
      Persistent = true;
    };
  };

  # TODO: Add offsite backup push (rsync/rclone to B2, S3, etc.)
  # Example systemd service that runs after pg-backup:
  # systemd.services.offsite-backup = {
  #   after = [ "pg-backup.service" ];
  #   serviceConfig.ExecStart = "${pkgs.rclone}/bin/rclone sync ${backupDir} remote:backups";
  # };
}
