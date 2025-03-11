{
  config,
  lib,
  pkgs,
  ...
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
    
    # Helper function for uv environment activation
    initExtra = ''
      # Better history management
      export HISTSIZE=10000
      export HISTFILESIZE=10000
      export HISTCONTROL=ignoredups:erasedups
      
      # Default editor
      export EDITOR=nvim
      
      # Use colored output by default
      alias grep='grep --color=auto'
      alias diff='diff --color=auto'
      alias ip='ip -color=auto'
    '';
  };

  # Tmux configuration
  programs.tmux = {
    enable = true;
    shortcut = "a";
    terminal = "tmux-256color";
    escapeTime = 10;
    historyLimit = 10000;
    keyMode = "vi";
    extraConfig = ''
      # Enable mouse support
      set -g mouse on
      
      # Start window numbering at 1
      set -g base-index 1
      set -g pane-base-index 1
      
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
      core.editor = "nvim";
      color.ui = "auto";
    };
    # User identity must be specified by user-specific configuration
  };

  # Common command-line utilities
  home.packages = with pkgs; [
    bat             # Better cat
    eza             # Better ls 
    fd              # Better find
    ripgrep         # Better grep
    fzf             # Fuzzy finder
    jq              # JSON processor
    htop            # Process viewer
    btop            # Process viewer
    
    # Development tools
    gnumake
    gcc
    
    # Python development
    uv              # Modern Python package manager
    ruff            # Python linter and formatter
    # nodePackages.pyright # Python language server
    
    # Nix development
    nixd            # Nix language server
    nixpkgs-fmt     # Nix formatter
  ];
}
