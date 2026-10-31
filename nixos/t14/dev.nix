# Development toolchain and desktop applications. The host file stays about
# hardware and the desktop session. Per-project pins live in each repo's
# shell.nix via direnv; this is what the editor and shell expect to exist.
{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  # Prebuilt binaries from mise, npm and friends expect an FHS loader.
  programs.nix-ld.enable = true;

  virtualisation.docker.enable = true;
  users.users.tim.extraGroups = [ "docker" ];

  environment.systemPackages = with pkgs; [
    # Runtimes. Projects pin their own versions in shell.nix; mise covers
    # repos that ship .tool-versions or .nvmrc.
    mise
    nodejs
    (python3.withPackages (ps: [ ps.pytest ]))
    go
    rustc
    cargo
    clippy
    rustfmt
    ruby_3_4
    elixir
    erlang
    clojure
    leiningen
    sbcl
    guile
    chez
    zig
    ghc
    crystal
    flutter
    terraform

    # Language servers
    typescript
    typescript-language-server
    vscode-langservers-extracted
    tailwindcss-language-server
    vue-language-server
    gopls
    rust-analyzer
    clang-tools
    lua-language-server
    clojure-lsp
    elixir-ls
    pyright
    ruby-lsp
    nil

    # Formatters and linters Doom modules look for
    shellcheck
    shfmt
    stylua
    nixfmt
    html-tidy
    js-beautify
    stylelint
    ruff
    gomodifytags
    gotests
    gore

    # CLI
    claude-code
    bat
    jq
    btop
    gh
    delta
    neovim
    helix
    glow
    cloc
    hugo
    typst
    pandoc
    imagemagick
    universal-ctags
    poppler-utils
    ffmpegthumbnailer
    nmap
    age
    sops
    ansible
    awscli2
    kubectl
    postgresql
    texinfo

    # Mail and media for mu4e and elfeed
    mu
    isync
    mpv
    yt-dlp

    # Desktop
    spotify
    discord
    brave
  ];
}
