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
    btop
    duf
    dust
    fd
    gcc
    glow
    gnumake
    htop
    jq
    nixd
    nixpkgs-fmt
    ripgrep
    ruff
    shfmt
    tmux
    uv
    viddy
    vivid
    just

    google-cloud-sdk-gce
    hcp
    terraform
    terragrunt
    vault-bin

    claude-code
    (python313.withPackages
      (ps: [ ps.llm ps.llm-anthropic ]))

    zig_0_14
  ];
}
