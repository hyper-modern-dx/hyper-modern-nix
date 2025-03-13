{ config
, lib
, pkgs
, ...
}:
let
  # Access Stylix colors
  colors = config.lib.stylix.colors.withHashtag;
in
{
  # Shared CLI tooling configuration across all users and systems
  # This provides a baseline of command-line tools and their configurations

  imports = [
    ../atuin
    ../emacs
    ../starship
    ../tmux
  ];

  programs.bash = {
    enable = true;
    shellAliases = {
      update = "sudo nixos-rebuild switch";
      update-flake = "nix flake update && sudo nixos-rebuild switch";
    };

    historyControl = [ "ignoredups" "erasedups" ];
    historyFileSize = 10000;
    historySize = 10000;

    sessionVariables = {
      EDITOR = "nvim";
    };

    initExtra = ''
      export TERM=xterm-256color
      export COLORTERM=truecolor
      export COLORFGBG="15;0"
    '';
  };

  programs.git = {
    enable = true;

    extraConfig = {
      branch.sort = "-committerdate";
      color.ui = "auto";
      column.ui = "auto";
      commit.verbose = true;
      help.autocorrect = "prompt";
      init.defaultBranch = "main";
      pull.rebase = true;
      tag.sort = "version:refname";

      fetch = {
        prune = true;
        pruneTags = true;
      };

      push = {
        default = "simple";
        autoSetupRemote = true;
        followTags = true;
      };

      rebase = {
        autoSquash = true;
        autoStash = true;
        updateRefs = true;
      };
    };
  };

  programs.eza = {
    enable = true;
  };

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
  };

  # Modern alternative to cat
  programs.bat = {
    enable = true;
  };

  # Common command-line utilities (removing duplicates and using programs options where available)
  home.packages = with pkgs; [
    awscli2
    beautysh
    biome
    btop
    bun
    cacert
    clang-tools
    clang-tools_19
    direnv
    duf
    dust
    fd
    fd
    gcc
    glow
    gnumake
    google-cloud-sdk-gce
    hclfmt
    hcp
    htop
    jq
    just
    nerd-fonts.hack
    nixd
    nixpkgs-fmt
    nodePackages_latest.nodejs
    nodePackages_latest.prettier
    nodePackages_latest.prettier
    nodePackages_latest.typescript-language-server
    nodePackages_latest.vscode-langservers-extracted
    openssl
    openssl.dev
    pkg-config
    python313Packages.python-lsp-server
    ripgrep
    ripgrep
    rubocop
    ruby_3_1
    ruff
    ruff
    shellcheck
    shfmt
    shfmt
    solargraph
    statix
    taplo
    terraform
    terragrunt
    toml-sort
    tree-sitter
    treefmt
    typescript
    typescript-language-server
    uv
    vault-bin
    viddy
    vivid
    yarn
    zig
    zls

    # AI stuff...
    claude-code
    (python313.withPackages
      (ps: [ ps.llm ps.llm-anthropic ]))
  ];
}
