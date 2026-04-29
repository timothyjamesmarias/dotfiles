{ config, pkgs, lib, ... }:

{
  boot.kernel.sysctl = {
    # Reverse path filtering — drop packets with spoofed source addresses
    "net.ipv4.conf.all.rp_filter" = 1;
    "net.ipv4.conf.default.rp_filter" = 1;

    # SYN flood protection
    "net.ipv4.tcp_syncookies" = 1;

    # Disable ICMP redirects
    "net.ipv4.conf.all.accept_redirects" = 0;
    "net.ipv4.conf.default.accept_redirects" = 0;
    "net.ipv4.conf.all.send_redirects" = 0;
    "net.ipv6.conf.all.accept_redirects" = 0;

    # Ignore ICMP broadcast requests
    "net.ipv4.icmp_echo_ignore_broadcasts" = 1;

    # Log martian packets
    "net.ipv4.conf.all.log_martians" = 1;
  };

  # Disable unused kernel modules
  boot.blacklistedKernelModules = [
    "dccp"     # Datagram Congestion Control Protocol
    "sctp"     # Stream Control Transmission Protocol
    "rds"      # Reliable Datagram Sockets
    "tipc"     # Transparent Inter-Process Communication
  ];
}
