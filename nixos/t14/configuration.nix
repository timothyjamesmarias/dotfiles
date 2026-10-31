{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./dev.nix
    # EWM (Emacs Wayland Manager): ships its own NixOS module and login session.
    "${builtins.fetchGit {
      url = "https://codeberg.org/ezemtsov/ewm.git";
      ref = "master";
      rev = "21d3c95e381fe3d8d3090f11d659796b9655f01e";
    }}/nix/service.nix"
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # Lenovo's DSDT references a dGPU this model lacks; the kernel logs it as an
  # ACPI error on every boot. Keep errors off the console, they stay in journald.
  boot.consoleLogLevel = 3;

  networking.hostName = "t14";
  networking.networkmanager.enable = true;
  # The router at 192.168.1.1 drops AAAA queries instead of answering them, so
  # every getaddrinfo() waits out the 5s glibc timeout. Resolve directly instead.
  networking.networkmanager.dns = "none";
  networking.nameservers = [ "1.1.1.1" "9.9.9.9" ];

  hardware.enableRedistributableFirmware = true;
  hardware.graphics.enable = true;
  services.fwupd.enable = true;
  services.thermald.enable = true;

  zramSwap.enable = true;

  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  users.users.tim = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" "input" ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keyFiles = [ ./tim.pub ];
  };
  programs.zsh.enable = true;

  services.openssh.enable = true;

  # Sway is the bootstrap session until EWM is packaged and proven.
  programs.sway.enable = true;
  services.greetd = {
    enable = true;
    settings.default_session.command = "${pkgs.tuigreet}/bin/tuigreet --time --remember --remember-session --sessions /run/current-system/sw/share/wayland-sessions";
  };
  security.pam.services.swaylock = { };

  # SSH agent at login, the keychain-style setup from the Mac: gcr-ssh-agent is
  # socket-activated, exports SSH_AUTH_SOCK to the systemd user session, lists
  # every ~/.ssh/*.pub and loads the private key on first use. Passphrases can
  # be saved in the login keyring, which PAM unlocks at greetd login.
  services.gnome.gcr-ssh-agent.enable = true;
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.greetd.enableGnomeKeyring = true;
  # Expose sway.desktop and ewm.desktop to tuigreet.
  environment.pathsToLink = [ "/share/wayland-sessions" ];

  programs.ewm.enable = true;
  # EWM targets nixpkgs unstable, which names 0.3 libdisplay-info_0_3. On 26.05
  # the plain attribute is already 0.3.0, so alias it.
  nixpkgs.overlays = [ (final: prev: { libdisplay-info_0_3 = prev.libdisplay-info; }) ];

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  hardware.bluetooth.enable = true;
  services.tlp.enable = true;

  # Ported from karabiner.json: caps as control, z-hold symbol layer,
  # tab-hold nav layer. The app-conditional C-n/C-p/C-w rules were dropped.
  services.keyd.enable = true;
  services.keyd.keyboards.default = {
    ids = [ "*" ];
    settings = {
      main = {
        capslock = "leftcontrol";
        z = "overloadt(symbols, z, 150)";
        tab = "overload(nav, tab)";
      };
      symbols = {
        j = "S-9";
        k = "S-0";
        l = "leftbrace";
        semicolon = "rightbrace";
        h = "S-leftbrace";
        apostrophe = "S-rightbrace";
        u = "S-comma";
        i = "S-dot";
        o = "minus";
        m = "S-minus";
        p = "equal";
        n = "S-backslash";
      };
      nav = {
        h = "left";
        j = "down";
        k = "up";
        l = "right";
      };
    };
  };

  fonts.packages = with pkgs; [ nerd-fonts.jetbrains-mono nerd-fonts.symbols-only ];

  environment.systemPackages = with pkgs; [
    # EWM: on-demand X11 apps and an Xcursor theme the compositor can load
    xwayland-satellite
    adwaita-icon-theme
    git
    stow
    direnv
    emacs-pgtk
    foot
    firefox
    wl-clipboard
    mako
    swaylock
    swayidle
    grim
    slurp
    pamixer
    brightnessctl
    ripgrep
    fd
    fzf
    gnumake
    gcc
    cmake
    libtool
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  system.stateVersion = "26.05";
}
