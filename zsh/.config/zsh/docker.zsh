# --- Docker aliases and Utilities ---

# Docker ps caching for improved performance
_docker_ps_cache=""
_docker_ps_cache_time=0
_docker_ps_all_cache=""
_docker_ps_all_cache_time=0

# Cache running containers (docker ps)
docker_ps_cached() {
  local current=$(date +%s)
  local ttl=3  # 3 second cache

  if [[ -z "$_docker_ps_cache" ]] || (( current - _docker_ps_cache_time > ttl )); then
    _docker_ps_cache=$(docker ps --format 'table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Status}}')
    _docker_ps_cache_time=$current
  fi

  echo "$_docker_ps_cache"
}

# Cache all containers (docker ps -a)
docker_ps_all_cached() {
  local current=$(date +%s)
  local ttl=3

  if [[ -z "$_docker_ps_all_cache" ]] || (( current - _docker_ps_all_cache_time > ttl )); then
    _docker_ps_all_cache=$(docker ps -a --format 'table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Status}}')
    _docker_ps_all_cache_time=$current
  fi

  echo "$_docker_ps_all_cache"
}

# Fuzzy exec into running container
docker_exec_fzf() {
  local container
  container=$(docker_ps_cached | \
    tail -n +2 | \
    fzf --header='Select container to exec into' \
        --preview 'docker inspect $(echo {} | awk "{print \$1}")' \
        --preview-window=right:60%) || return

  if [ -n "$container" ]; then
    local id=$(echo "$container" | awk '{print $1}')
    local shell="${1:-bash}"

    echo "Attempting to exec with $shell..."
    docker exec -it "$id" "$shell" 2>/dev/null || \
    docker exec -it "$id" sh
  fi
}

# Fuzzy view container logs
docker_logs_fzf() {
  local container
  container=$(docker_ps_all_cached | \
    tail -n +2 | \
    fzf --header='Select container to view logs' \
        --preview 'docker logs --tail 100 $(echo {} | awk "{print \$1}") 2>&1' \
        --preview-window=right:70%) || return

  if [ -n "$container" ]; then
    local id=$(echo "$container" | awk '{print $1}')
    echo -n "Follow logs? (y/n) [default=y]: "
    read follow

    case "$follow" in
      n|N) docker logs "$id" ;;
      *) docker logs -f "$id" ;;
    esac
  fi
}

# Fuzzy stop containers
docker_stop_fzf() {
  local containers
  containers=$(docker_ps_cached | \
    tail -n +2 | \
    fzf -m --header='Select containers to stop (TAB to select multiple)' \
           --preview 'docker inspect $(echo {} | awk "{print \$1}")' \
           --preview-window=right:60%) || return

  if [ -n "$containers" ]; then
    echo "$containers" | awk '{print $1}' | xargs docker stop
    echo "Stopped containers"
  fi
}

# Stop all running containers
docker_stop_all() {
  local count=$(docker ps -q | wc -l | tr -d ' ')

  if [ "$count" -eq 0 ]; then
    echo "No running containers"
    return
  fi

  echo "Found $count running container(s)"
  docker ps --format 'table {{.ID}}\t{{.Names}}\t{{.Image}}'
  echo ""
  echo -n "Stop all running containers? (y/n): "
  read confirm

  if [[ "$confirm" =~ ^[Yy]$ ]]; then
    docker stop $(docker ps -q)
    echo "All containers stopped"
  else
    echo "Cancelled"
  fi
}

# Fuzzy remove containers
docker_rm_fzf() {
  local containers
  containers=$(docker_ps_all_cached | \
    tail -n +2 | \
    fzf -m --header='Select containers to remove (TAB to select multiple)' \
           --preview 'docker inspect $(echo {} | awk "{print \$1}")' \
           --preview-window=right:60%) || return

  if [ -n "$containers" ]; then
    echo "$containers"
    echo ""
    echo -n "Remove these containers? (y/n): "
    read confirm

    if [[ "$confirm" =~ ^[Yy]$ ]]; then
      echo "$containers" | awk '{print $1}' | xargs docker rm
      echo "Containers removed"
    fi
  fi
}

# Fuzzy inspect container details
docker_inspect_fzf() {
  local container
  container=$(docker_ps_all_cached | \
    tail -n +2 | \
    fzf --header='Select container to inspect' \
        --preview 'docker inspect $(echo {} | awk "{print \$1}")' \
        --preview-window=right:70%) || return

  if [ -n "$container" ]; then
    local id=$(echo "$container" | awk '{print $1}')

    # Use jq if available for pretty output, otherwise raw
    if command -v jq >/dev/null 2>&1; then
      docker inspect "$id" | jq '.'
    else
      docker inspect "$id"
    fi
  fi
}

# Fuzzy select and manage docker images
docker_image_fzf() {
  local image
  image=$(docker images --format 'table {{.Repository}}\t{{.Tag}}\t{{.ID}}\t{{.Size}}\t{{.CreatedSince}}' | \
    tail -n +2 | \
    fzf --header='Select image (r=remove, i=inspect, n=run)' \
        --preview 'docker inspect $(echo {} | awk "{print \$3}")' \
        --preview-window=right:60%) || return

  if [ -n "$image" ]; then
    local id=$(echo "$image" | awk '{print $3}')
    local repo=$(echo "$image" | awk '{print $1}')
    local tag=$(echo "$image" | awk '{print $2}')

    echo "Selected: $repo:$tag ($id)"
    echo -n "Action? (r)emove, (i)nspect, (n)ew container [default=inspect]: "
    read action

    case "$action" in
      r)
        echo -n "Remove image $repo:$tag? (y/n): "
        read confirm
        [[ "$confirm" =~ ^[Yy]$ ]] && docker rmi "$id"
        ;;
      n)
        echo -n "Enter run command args (e.g., -p 8080:80): "
        read args
        docker run -it $args "$repo:$tag"
        ;;
      i|"")
        if command -v jq >/dev/null 2>&1; then
          docker inspect "$id" | jq '.'
        else
          docker inspect "$id"
        fi
        ;;
      *) echo "Cancelled" ;;
    esac
  fi
}

# View port mappings for containers
docker_port_fzf() {
  local container
  container=$(docker_ps_cached | sed '1s/Status/Ports/' | \
    tail -n +2 | \
    fzf --header='Select container to view ports' \
        --preview 'docker port $(echo {} | awk "{print \$1}")' \
        --preview-window=down:8) || return

  if [ -n "$container" ]; then
    local id=$(echo "$container" | awk '{print $1}')
    docker port "$id"
  fi
}

# Interactive container stats
docker_stats_fzf() {
  local container
  container=$(docker_ps_cached | \
    tail -n +2 | \
    fzf -m --header='Select containers to monitor (TAB for multiple)' \
           --preview 'docker stats --no-stream $(echo {} | awk "{print \$1}")' \
           --preview-window=down:8) || return

  if [ -n "$container" ]; then
    echo "$container" | awk '{print $1}' | xargs docker stats
  fi
}

# Fuzzy restart containers
docker_restart_fzf() {
  local containers
  containers=$(docker_ps_cached | \
    tail -n +2 | \
    fzf -m --header='Select containers to restart (TAB for multiple)' \
           --preview 'docker inspect $(echo {} | awk "{print \$1}")' \
           --preview-window=right:60%) || return

  if [ -n "$containers" ]; then
    echo "$containers" | awk '{print $1}' | xargs docker restart
    echo "Containers restarted"
  fi
}

# Interactive prune - cleanup unused resources
docker_prune_fzf() {
  echo "Docker Cleanup Menu"
  echo "──────────────────"
  echo "1. Prune stopped containers"
  echo "2. Prune unused images"
  echo "3. Prune unused volumes"
  echo "4. Prune unused networks"
  echo "5. Prune everything (system)"
  echo "6. Cancel"
  echo ""
  echo -n "Select option: "
  read option

  case "$option" in
    1) docker container prune ;;
    2) docker image prune -a ;;
    3) docker volume prune ;;
    4) docker network prune ;;
    5) docker system prune -a --volumes ;;
    *) echo "Cancelled" ;;
  esac
}

# Docker compose utilities
docker_compose_fzf() {
  if [ ! -f "docker-compose.yml" ] && [ ! -f "docker-compose.yaml" ] && [ ! -f "compose.yml" ] && [ ! -f "compose.yaml" ]; then
    echo "No docker-compose.yml found in current directory"
    return 1
  fi

  # Get list of services
  local service
  service=$(docker compose config --services 2>/dev/null | \
    fzf --header='Select service (u=up, d=down, r=restart, l=logs, e=exec)') || return

  if [ -n "$service" ]; then
    echo "Selected: $service"
    echo -n "Action? (u)p, (d)own, (r)estart, (l)ogs, (e)xec [default=logs]: "
    read action

    case "$action" in
      u) docker compose up -d "$service" ;;
      d) docker compose down "$service" ;;
      r) docker compose restart "$service" ;;
      e)
        local shell="${1:-bash}"
        docker compose exec "$service" "$shell" 2>/dev/null || \
        docker compose exec "$service" sh
        ;;
      l|"") docker compose logs -f "$service" ;;
      *) echo "Cancelled" ;;
    esac
  fi
}

# Quick container info view
docker_ps_tree() {
  docker ps --format 'table {{.Names}}\t{{.Image}}\t{{.Status}}\t{{.Ports}}' | \
    awk 'NR==1 {print; print ""; next} {print}' | \
    column -t -s $'\t'
}

# Find containers by name pattern
docker_find() {
  if [ -z "$1" ]; then
    echo "Usage: docker_find <pattern>"
    return 1
  fi

  docker ps -a --filter "name=$1" --format 'table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Status}}'
}

# Copy files from container
docker_cp_fzf() {
  local container
  container=$(docker_ps_all_cached | \
    tail -n +2 | \
    fzf --header='Select container to copy from') || return

  if [ -n "$container" ]; then
    local id=$(echo "$container" | awk '{print $1}')
    echo -n "Enter source path in container: "
    read src
    echo -n "Enter destination path (default=.): "
    read dest
    dest="${dest:-.}"

    docker cp "$id:$src" "$dest"
    echo "Copied $src from $id to $dest"
  fi
}

# Network inspection
docker_network_fzf() {
  local network
  network=$(docker network ls --format 'table {{.ID}}\t{{.Name}}\t{{.Driver}}\t{{.Scope}}' | \
    tail -n +2 | \
    fzf --header='Select network to inspect' \
        --preview 'docker network inspect $(echo {} | awk "{print \$1}")' \
        --preview-window=right:60%) || return

  if [ -n "$network" ]; then
    local id=$(echo "$network" | awk '{print $1}')

    if command -v jq >/dev/null 2>&1; then
      docker network inspect "$id" | jq '.'
    else
      docker network inspect "$id"
    fi
  fi
}

# Volume inspection and management
docker_volume_fzf() {
  local volume
  volume=$(docker volume ls --format 'table {{.Name}}\t{{.Driver}}\t{{.Scope}}' | \
    tail -n +2 | \
    fzf --header='Select volume (i=inspect, r=remove)' \
        --preview 'docker volume inspect $(echo {} | awk "{print \$1}")' \
        --preview-window=right:60%) || return

  if [ -n "$volume" ]; then
    local name=$(echo "$volume" | awk '{print $1}')

    echo "Selected: $name"
    echo -n "Action? (i)nspect, (r)emove [default=inspect]: "
    read action

    case "$action" in
      r)
        echo -n "Remove volume $name? (y/n): "
        read confirm
        [[ "$confirm" =~ ^[Yy]$ ]] && docker volume rm "$name"
        ;;
      i|"")
        if command -v jq >/dev/null 2>&1; then
          docker volume inspect "$name" | jq '.'
        else
          docker volume inspect "$name"
        fi
        ;;
      *) echo "Cancelled" ;;
    esac
  fi
}

# Basic docker aliases
alias dps='docker ps'
alias dpsa='docker ps -a'
alias di='docker images'
alias drm='docker rm'
alias drmi='docker rmi'
alias dstop='docker stop'
alias dstart='docker start'
alias dlogs='docker logs'
alias dexec='docker exec -it'
alias dinspect='docker inspect'

# Docker compose aliases
alias dc='docker compose'
alias dcu='docker compose up -d'
alias dcd='docker compose down'
alias dcl='docker compose logs -f'
alias dcr='docker compose restart'
alias dcps='docker compose ps'

# Fuzzy docker aliases
alias dex='docker_exec_fzf'
alias dl='docker_logs_fzf'
alias dst='docker_stop_fzf'
alias dsa='docker_stop_all'
alias drx='docker_rm_fzf'
alias dins='docker_inspect_fzf'
alias dim='docker_image_fzf'
alias dport='docker_port_fzf'
alias dstat='docker_stats_fzf'
alias drs='docker_restart_fzf'
alias dprune='docker_prune_fzf'
alias dcomp='docker_compose_fzf'
alias dcp='docker_cp_fzf'
alias dnet='docker_network_fzf'
alias dvol='docker_volume_fzf'
alias dtree='docker_ps_tree'
