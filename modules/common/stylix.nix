{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  # Define colors (based on your Ono-Sendai theme)
  colors = {
    # Base colors
    base00 = "101216"; # Dark background
    base01 = "161B22"; # Lighter background (highlights)
    base02 = "1B1F23"; # Selection background
    base03 = "5C6370"; # Comments, invisibles
    base04 = "676E7D"; # Dark foreground
    base05 = "bababa"; # Default foreground
    base06 = "d3d7cf"; # Light foreground
    base07 = "ffffff"; # Light background

    # Colors
    base08 = "f78166"; # Red (variables)
    base09 = "f49b4f"; # Orange (integers, booleans)
    base0A = "FDA656"; # Yellow (classes, search)
    base0B = "8ddb8c"; # Green (strings)
    base0C = "96cffe"; # Cyan (escape chars, regex)
    base0D = "539bf5"; # Blue (functions, methods)
    base0E = "d5b7f4"; # Magenta (keywords)
    base0F = "AB5DFF"; # Purple (deprecated)
  };

  # Create the Stylix scheme from our colors
  scheme = {
    slug = "ono-sendai-hyper-modern";
    name = "Ono-Sendai Hyper Modern";
    author = "b7r6";
    inherit colors;
  };
in
{
  stylix = {
    # Use our custom color scheme
    base16Scheme = scheme;

    # Set default fonts (system-ui provides good cross-platform compatibility)
    fonts = {
      monospace = {
        name = "JetBrains Mono";
        package = pkgs.jetbrains-mono;
      };
      sansSerif = {
        name = "Inter";
        package = pkgs.inter;
      };
      serif = {
        name = "Noto Serif";
        package = pkgs.noto-fonts;
      };
    };

    # Terminal-specific configurations
    targets = {
      # Enable true color support but maintain ANSI compatibility
      tmux.enable = true;
      tmux.extraConfig = ''
        # Enable 24-bit color support
        set -g default-terminal "tmux-256color"
        set -ag terminal-overrides ",xterm-256color:RGB"
        set -ag terminal-overrides ",*256col*:RGB"
        set -ag terminal-overrides ",alacritty:RGB"
        set -ag terminal-overrides ",wezterm:RGB"
        set -ag terminal-overrides ",ghostty:RGB"

        # Ensure consistent behavior with different terminals
        set -g escape-time 10
        set -g focus-events on
      '';

      # Wezterm configuration
      wezterm.enable = true;
      wezterm.extraConfig = ''
        return {
          -- Ensure color compatibility
          enable_wayland = false,
          front_end = "WebGpu",
          webgpu_power_preference = "HighPerformance",
          
          -- Disable features that might cause compatibility issues
          enable_kitty_keyboard = false,
          
          -- Set fallback for environments without true-color
          force_reverse_video_cursor = true,
        }
      '';

      # Ensure Emacs theme compatibility
      emacs.enable = true;
      vim.enable = true;

      # Disable targets that aren't used to avoid configuration conflicts
      alacritty.enable = false;
      kitty.enable = false;
    };

    # Cursor size and animation options (conservative defaults)
    cursor = {
      size = 16;
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
    };

    # Opacity settings (1.0 = fully opaque)
    opacity.terminal = 1.0;
    opacity.desktop = 1.0;
    opacity.applications = 1.0;
  };
}