{ config, pkgs, ... }:
{
  # # Let Home Manager install and manage itself
  # programs.home-manager.enable = true;

  # # Home Manager needs a bit of information about you and the paths it should manage
  # home.username = "b7r6";
  # home.homeDirectory = "/home/b7r6";

  # # Shell configuration
  # programs.bash = {
  #   enable = true;
  #   shellAliases = {
  #     ll = "ls -la";
  #     update = "sudo nixos-rebuild switch";
  #     update-flake = "nix flake update && sudo nixos-rebuild switch";
  #     gs = "git status";
  #     gd = "git diff";
  #   };
  #   initExtra = ''
  #     # Set editor to neovim
  #     export EDITOR=nvim

  #     # Python virtual environment handling with uv
  #     export UV_SYSTEM_PYTHON="$(which python3)"

  #     # Better history management
  #     export HISTSIZE=10000
  #     export HISTFILESIZE=10000
  #     export HISTCONTROL=ignoredups:erasedups

  #     # Modern command line prompt
  #     PS1='\[\033[1;36m\]\u\[\033[1;37m\]@\[\033[1;32m\]\h\[\033[1;37m\]:\[\033[1;34m\]\w\[\033[0m\]\$ '
  #   '';
  # };

  # # Git configuration
  # programs.git = {
  #   enable = true;
  #   userName = "b7r6";
  #   userEmail = "b7r6@pm.me";
  #   extraConfig = {
  #     init.defaultBranch = "main";
  #     pull.rebase = true;
  #     push.default = "current";
  #   };
  # };

  # # Tmux configuration
  # programs.tmux = {
  #   enable = true;
  #   shortcut = "a";
  #   terminal = "tmux-256color";
  #   escapeTime = 10;
  #   historyLimit = 10000;
  #   extraConfig = ''
  #     # Enable mouse support
  #     set -g mouse on

  #     # Start window numbering at 1
  #     set -g base-index 1
  #     set -g pane-base-index 1

  #     # True color settings
  #     set -ag terminal-overrides ",*256col*:RGB"
  #     set -ag terminal-overrides ",alacritty:RGB"
  #     set -ag terminal-overrides ",wezterm:RGB"
  #     set -ag terminal-overrides ",ghostty:RGB"

  #     # Reload configuration
  #     bind r source-file ~/.config/tmux/tmux.conf \; display "Reloaded!"

  #     # Split panes using | and -
  #     bind | split-window -h
  #     bind - split-window -v
  #     unbind '"'
  #     unbind %
  #   '';
  # };

  # # Vim/Neovim configuration
  # programs.neovim = {
  #   enable = true;
  #   viAlias = true;
  #   vimAlias = true;
  #   defaultEditor = true;
  #   plugins = with pkgs.vimPlugins; [
  #     nvim-treesitter
  #     telescope-nvim
  #     plenary-nvim
  #     lualine-nvim
  #     nvim-lspconfig
  #     nvim-cmp
  #     cmp-nvim-lsp
  #     cmp-buffer
  #     cmp-path
  #   ];
  #   extraLuaConfig = ''
  #     -- Basic settings
  #     vim.opt.number = true
  #     vim.opt.relativenumber = true
  #     vim.opt.shiftwidth = 2
  #     vim.opt.tabstop = 2
  #     vim.opt.expandtab = true
  #     vim.opt.termguicolors = true
  #     vim.opt.ignorecase = true
  #     vim.opt.smartcase = true

  #     -- LSP Configuration
  #     local lspconfig = require('lspconfig')

  #     -- Nix
  #     lspconfig.nixd.setup{}

  #     -- Python
  #     lspconfig.pyright.setup{}

  #     -- Treesitter
  #     require('nvim-treesitter.configs').setup {
  #       ensure_installed = { "lua", "vim", "vimdoc", "python", "nix", "bash" },
  #       highlight = { enable = true },
  #     }

  #     -- Keymaps
  #     vim.g.mapleader = " "
  #     vim.keymap.set('n', '<leader>ff', require('telescope.builtin').find_files, { desc = 'Find Files' })
  #     vim.keymap.set('n', '<leader>fg', require('telescope.builtin').live_grep, { desc = 'Find Text' })
  #     vim.keymap.set('n', '<leader>fb', require('telescope.builtin').buffers, { desc = 'Find Buffers' })
  #     vim.keymap.set('n', '<leader>fh', require('telescope.builtin').help_tags, { desc = 'Find Help' })
  #   '';
  # };

  # # Python development environment with uv
  # home.packages = with pkgs; [
  #   bat
  #   fd
  #   fzf
  #   ghostty
  #   htop
  #   jq
  #   nodePackages.pyright # For better language server support
  #   ripgrep
  #   ruff
  #   uv
  # ];

  # # This value determines the Home Manager release that your
  # # configuration is compatible with. This helps avoid breakage
  # # when a new Home Manager release introduces backwards
  # # incompatible changes.
  # #
  # # You should not change this value, even if you update Home Manager. If you do
  # # want to update the value, then make sure to first check the Home Manager
  # # release notes.

  # home.stateVersion = "24.11"; # Please read the comment before changing.
}
