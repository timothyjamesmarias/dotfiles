{ config, pkgs, lib, ... }:

{
  networking.firewall = {
    enable = true;

    # Public-facing: only HTTP and HTTPS
    allowedTCPPorts = [ 80 443 ];

    # SSH is allowed by default when openssh is enabled.
    # To restrict SSH to specific IPs, use:
    # extraCommands = ''
    #   iptables -A INPUT -p tcp --dport 22 -s YOUR_IP/32 -j ACCEPT
    #   iptables -A INPUT -p tcp --dport 22 -j DROP
    # '';

    # No UDP services exposed
    allowedUDPPorts = [];
  };
}
