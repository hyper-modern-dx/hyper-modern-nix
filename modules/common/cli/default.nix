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

  # Terminal configuration
  programs.wezterm = {
    enable = true;
    extraConfig = ''
    '';
  };

  # Shell configuration
  programs.bash = {
    enable = true;
    shellAliases = {
      "z" = "zoxide";

      # System operations
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
    '';
  };

  # Tmux configuration
  programs.tmux = {
    enable = true;
    prefix = "C-o";
    terminal = "xterm-direct";
    escapeTime = 10;
    historyLimit = 10000;
    keyMode = "vi";
    baseIndex = 1;
    mouse = true;
    extraConfig = ''
      # Reload configuration
      bind r source-file ~/.config/tmux/tmux.conf \; display "Reloaded!"
      
      # Split panes using | and -
      bind | split-window -h
      bind - split-window -v
      unbind '"'
      unbind %
      
      # Shift arrow to switch windows
      bind -n S-Left  previous-window
      bind -n S-Right next-window
    '';
  };

  # Emacs configuration
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-nox;

    # Install necessary packages
    extraPackages = epkgs: [
      epkgs.base16-theme
      epkgs.vterm
      epkgs.magit
    ];

    # Comprehensive configuration to fix color issues
    extraConfig = ''
    '';
  };

  # Enable Stylix integration for Emacs
  stylix.targets.emacs.enable = true;

  # Neovim configuration
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    defaultEditor = true;
    plugins = with pkgs.vimPlugins; [
      nvim-treesitter
      telescope-nvim
      plenary-nvim
      lualine-nvim
      nvim-lspconfig
      nvim-cmp
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
    ];
    extraLuaConfig = ''
    '';
  };

  # Git configuration
  programs.git = {
    enable = true;
    extraConfig = {
      init.defaultBranch = "main";
      pull.rebase = true;
      push.default = "current";
      color.ui = "auto";
    };
    # User identity must be specified by user-specific configuration
  };

  # File listing utilities
  programs.eza = {
    enable = true;
  };

  # Fast directory navigation
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };

  # Starship prompt
  programs.starship = {
    enable = true;
    enableBashIntegration = true;

    settings = {
      format = "[//](fg:${colors.base03})$hostname$git_branch$directory[//](fg:${colors.base03})\n$character";

      hostname = {
        format = " [$hostname]($style) ";
        style = "${colors.base0E}"; # Magenta from base16
        ssh_only = false;
        disabled = false;
      };

      directory = {
        format = "[$path]($style) ";
        style = "${colors.base0D}"; # Blue from base16
        truncate_to_repo = true;
      };

      git_branch = {
        format = "[$branch]($style) ";
        style = "fg:${colors.base09}"; # Orange from base16
      };

      character = {
        format = "[//](fg:${colors.base03}) ";
        success_symbol = "[//](fg:${colors.base03})";
        error_symbol = "[//](fg:${colors.base08})"; # Red from base16
        disabled = false;
      };

      # Global settings
      scan_timeout = 10;
      add_newline = false;
    };
  };

  # Fuzzy finder
  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
  };

  # Modern alternative to cat
  programs.bat = {
    enable = true;
  };

  # McFly history search tool
  programs.mcfly = {
    enable = true;
    enableBashIntegration = true;
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
  ];
}
