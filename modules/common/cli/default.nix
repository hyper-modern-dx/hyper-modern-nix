{ config
, lib
, pkgs
, ...
}:

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
      # File operations
      ll = "ls -la";
      l = "ls -l";

      # System operations
      update = "sudo nixos-rebuild switch";
      update-flake = "nix flake update && sudo nixos-rebuild switch";

      # Git shortcuts
      gs = "git status";
      gd = "git diff";
      gl = "git log --oneline --graph --decorate --all -n 10";

      # Development
      py = "python3";
    };

    historyControl = [ "ignoredups" "erasedups" ];
    historyFileSize = 10000;
    historySize = 10000;

    sessionVariables = {
      EDITOR = "nvim";
    };

    # Helper function for uv environment activation
    initExtra = ''
      # Use colored output by default
      alias grep='grep --color=auto'
      alias diff='diff --color=auto'
      alias ip='ip -color=auto'
    '';
  };

  # Tmux configuration
  programs.tmux = {
    enable = true;
    prefix = "C-a";
    terminal = "tmux-256color";
    escapeTime = 10;
    historyLimit = 10000;
    keyMode = "vi";
    baseIndex = 1;
    mouse = true;
    extraConfig = ''
      # True color settings
      set -ag terminal-overrides ",*256col*:RGB"
      set -ag terminal-overrides ",alacritty:RGB"
      set -ag terminal-overrides ",wezterm:RGB"
      set -ag terminal-overrides ",ghostty:RGB"
      
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
      -- Basic settings
      vim.opt.number = true
      vim.opt.relativenumber = true
      vim.opt.shiftwidth = 2
      vim.opt.tabstop = 2
      vim.opt.expandtab = true
      vim.opt.termguicolors = true
      vim.opt.ignorecase = true
      vim.opt.smartcase = true
      
      -- LSP Configuration
      local lspconfig = require('lspconfig')
      
      -- Nix
      lspconfig.nixd.setup{}
      
      -- Python
      -- lspconfig.pyright.setup{}
      
      -- Treesitter
      require('nvim-treesitter.configs').setup {
        ensure_installed = { "lua", "vim", "vimdoc", "python", "nix", "bash" },
        highlight = { enable = true },
      }
      
      -- Keymaps
      vim.g.mapleader = " "
      vim.keymap.set('n', '<leader>ff', require('telescope.builtin').find_files, { desc = 'Find Files' })
      vim.keymap.set('n', '<leader>fg', require('telescope.builtin').live_grep, { desc = 'Find Text' })
      vim.keymap.set('n', '<leader>fb', require('telescope.builtin').buffers, { desc = 'Find Buffers' })
      vim.keymap.set('n', '<leader>fh', require('telescope.builtin').help_tags, { desc = 'Find Help' })
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
    emacs
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
