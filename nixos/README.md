# NixOS Server Configuration

Declarative NixOS configuration for a self-hosted compute server running Kotlin (Ktor) backends as Docker containers with GraalVM native images.

## Architecture

```
Internet → Cloudflare (DNS / DDoS / WAF)
  │
  └─ NixOS Host (firewall: 80/443 only)
      │
      ├─ Traefik (reverse proxy, auto TLS, ForwardAuth)
      │    ├─ api.domain.com      → app-container:8080
      │    ├─ sub.other.com       → app-container:8080
      │    └─ grafana.domain.com  → grafana:3000
      │
      ├─ App containers (isolated Docker networks)
      │    └─ GraalVM native images pulled from GHCR
      │
      ├─ PostgreSQL (NixOS-managed, single instance)
      │    ├─ PgBouncer (transaction-mode connection pooling)
      │    └─ One database + user per app, scoped pg_hba.conf
      │
      └─ Monitoring
           ├─ Prometheus (metrics)
           ├─ Grafana (dashboards)
           ├─ Loki (logs)
           └─ OTel Collector (receives OTLP from apps)
```

### Network Isolation

Each app container connects to `proxy-net` (so Traefik can route to it) and reaches PostgreSQL via `host.docker.internal`. Apps cannot communicate with each other. The monitoring stack runs on its own `monitoring-net`.

```
proxy-net        ← Traefik + all app containers
postgres-net     ← (reserved, exporters)
monitoring-net   ← Prometheus, Grafana, Loki, OTel Collector, exporters
```

## Prerequisites

- A server with NixOS installed (or a NixOS installer USB)
- `age` key pair for sops-nix secret encryption
- GitHub account with Container Registry access
- Cloudflare account for DNS and API token (Zone:DNS:Edit)
- Domain(s) pointed at the server's IP via Cloudflare

## Initial Setup

### 1. Generate hardware configuration

On the server:

```bash
nixos-generate-config --show-hardware-config > /tmp/hardware-configuration.nix
```

Copy the output to `nixos/hardware-configuration.nix`, replacing the placeholder.

### 2. Generate age key for secrets

```bash
# On the server
age-keygen -o /var/lib/sops-nix/key.txt

# Note the public key (age1xxx...) — you'll need it next
```

### 3. Configure secrets

```bash
# Update secrets/.sops.yaml with the public key from step 2
# Then create the encrypted secrets file:
cp secrets/secrets.example.yaml secrets/secrets.yaml
sops secrets/secrets.yaml
# Fill in real values, save, and exit
```

### 4. Customize configuration

Search for `TODO` across all `.nix` files and fill in:
- Hostname, timezone, SSH public keys (`modules/base.nix`)
- Domains and email addresses (`modules/traefik.nix`, `modules/monitoring.nix`)
- GitHub username (`modules/docker.nix`, `apps/example-api.nix`)
- PostgreSQL tuning for your hardware (`modules/postgresql.nix`)

### 5. First deploy

```bash
cd /path/to/nixos
sudo nixos-rebuild switch --flake .#server
```

## Adding a New App

1. **Copy the template:**

```bash
cp apps/_template.nix apps/my-app.nix
```

2. **Edit `apps/my-app.nix`** — fill in `appName`, `domain`, `imageSha`, and `ghOrg`.

3. **Add the database** to `modules/postgresql.nix`:

```nix
appDatabases = [
  { name = "example-api"; user = "example_api"; dbName = "example_api"; }
  { name = "my-app";      user = "my_app";      dbName = "my_app"; }  # ← new
];
```

4. **Import the app** in `configuration.nix`:

```nix
./apps/example-api.nix
./apps/my-app.nix          # ← new
```

5. **Create the database** (for immediate use before rebuild):

```bash
./scripts/db-create.sh my-app my_app my_app
```

6. **Deploy:**

```bash
sudo nixos-rebuild switch --flake .#server
```

## Managing Secrets

Secrets are encrypted with sops-nix using age. The encrypted file is `secrets/secrets.yaml`.

```bash
# Edit secrets (decrypts in-place, re-encrypts on save)
sops secrets/secrets.yaml

# Add a new secret — just add a new key to the YAML

# After editing, rebuild to apply
sudo nixos-rebuild switch --flake .#server
```

To add app-specific secrets, add them to `secrets.yaml` and declare them in `modules/secrets.nix`:

```nix
sops.secrets."my-app/api-key" = { owner = "root"; };
```

Then reference in the app's nix file:

```nix
environmentFiles = [ config.sops.secrets."my-app/api-key".path ];
```

## Deploying Updates

### Via deploy script

```bash
# Get the new image SHA from your CI build output, then:
./scripts/deploy.sh example-api sha256:abc123...
```

The script updates the SHA in the app's nix file and runs `nixos-rebuild switch`.

### Manual

```bash
# Edit apps/example-api.nix — update imageSha
vim apps/example-api.nix

# Rebuild
sudo nixos-rebuild switch --flake .#server
```

### Rollback

```bash
# Instant rollback to previous generation
sudo nixos-rebuild switch --rollback
```

## Backup and Restore

### Automatic schedule

| What | When | Retention |
|------|------|-----------|
| PostgreSQL databases | Daily 3:00 AM | 30 days |
| Docker volumes | Weekly Sunday 4:00 AM | 30 days |

Backups are stored in `/var/backups/`.

### Manual operations

```bash
# List all backups
./scripts/backup-restore.sh list

# Restore a database
./scripts/backup-restore.sh restore-db example_api example_api-2025-04-28_030000.sql.gz

# Restore a volume
./scripts/backup-restore.sh restore-volume grafana-data grafana-data-2025-04-28_040000.tar.gz
```

### Offsite backup

Not configured by default. See the TODO in `modules/backups.nix` for an example using rclone to push to S3/B2.

## Monitoring and Alerting

### Grafana

Accessible at `grafana.yourdomain.com` (configured in `modules/monitoring.nix`). Default admin user is `admin`, password set via sops.

Pre-provisioned datasources:
- **Prometheus** — system and app metrics
- **Loki** — aggregated logs

### Metrics collected

| Source | Exporter | Metrics |
|--------|----------|---------|
| Host | node-exporter | CPU, memory, disk, network |
| PostgreSQL | postgres-exporter | Connections, queries, locks, replication |
| Traefik | Built-in | Request rates, latencies, error rates |
| Apps | OTel Collector | Custom app metrics via OTLP |

### App instrumentation

Apps send telemetry via OpenTelemetry. Each app container has these environment variables set:

```
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
OTEL_SERVICE_NAME=<app-name>
```

For Ktor, use the `ktor-server-opentelemetry` plugin. Traces, metrics, and logs are all routed through the OTel Collector to Prometheus (metrics) and Loki (logs).

### Viewing logs

```bash
# Container logs via Docker
docker logs <container-name>

# Aggregated logs via Loki (in Grafana)
# Navigate to Explore → Loki → {service_name="example-api"}

# Systemd service logs
journalctl -u docker-example-api.service
```

## Security Model

```
Layer 0  Cloudflare        DDoS protection, WAF, bot filtering
Layer 1  NixOS firewall    Only TCP 80/443 open to the world
Layer 2  SSH               Key-only auth, fail2ban (3 retries, 1h ban)
Layer 3  Kernel            sysctl hardening (syncookies, no redirects, rp_filter)
Layer 4  Traefik           TLS termination, HSTS, rate limiting, ForwardAuth
Layer 5  Auth service      JWT validation via Traefik ForwardAuth middleware
Layer 6  App-level         Authorization logic in your Kotlin code
Layer 7  PostgreSQL        Per-app user, scoped pg_hba.conf, no cross-db access
Layer 8  Docker networks   Apps isolated from each other
Layer 9  Secrets           Encrypted at rest via sops-nix (age), decrypted at boot
```

## Troubleshooting

### Container won't start

```bash
# Check systemd service status
systemctl status docker-example-api.service

# Check container logs
journalctl -u docker-example-api.service -n 50

# Verify the image exists
docker pull ghcr.io/you/example-api@sha256:...
```

### TLS certificate issues

```bash
# Check Traefik logs
docker logs traefik 2>&1 | grep -i acme

# Verify DNS resolution
dig api.example.com

# Check the ACME storage
docker exec traefik cat /acme/acme.json | jq '.letsencrypt'
```

### Database connection refused

```bash
# Is PostgreSQL running?
systemctl status postgresql

# Is PgBouncer running?
systemctl status pgbouncer

# Can a container reach the host?
docker exec example-api nc -zv host.docker.internal 6432

# Check pg_hba.conf
cat /etc/postgresql/pg_hba.conf
```

### Rebuild fails

```bash
# Dry-run to see what changed
nixos-rebuild dry-activate --flake .#server

# Build without switching (to see errors)
nixos-rebuild build --flake .#server

# Check nix evaluation
nix eval .#nixosConfigurations.server.config.networking.hostName
```

## File Structure

```
nixos/
├── flake.nix                   Flake entry point (inputs + outputs)
├── configuration.nix           Top-level imports
├── hardware-configuration.nix  Hardware-specific config (auto-generated)
├── modules/
│   ├── base.nix                Users, locale, base packages
│   ├── ssh.nix                 SSH hardening + fail2ban
│   ├── firewall.nix            iptables rules
│   ├── hardening.nix           Kernel sysctl + module blacklist
│   ├── docker.nix              Docker daemon + shared networks
│   ├── postgresql.nix          PostgreSQL + PgBouncer + app DB registry
│   ├── traefik.nix             Reverse proxy + TLS + ForwardAuth
│   ├── monitoring.nix          Prometheus, Grafana, Loki, OTel, exporters
│   ├── backups.nix             Scheduled pg_dump + volume backups
│   └── secrets.nix             sops-nix secret declarations
├── apps/
│   ├── _template.nix           Copy this for new apps
│   └── example-api.nix         Working example
├── scripts/
│   ├── deploy.sh               Update image SHA + rebuild
│   ├── db-create.sh            Create app database + user
│   └── backup-restore.sh       List and restore backups
└── secrets/
    ├── .sops.yaml              Age key config for sops
    └── secrets.example.yaml    Template for secrets.yaml
```
