{ config, lib, pkgs, ... }:
{
  wayland.windowManager.hyprland = {
    enable = true;
    
    settings = {
      # Monitor configuration
      monitor = [
        "eDP-1,1920x1080@60,0x0,1"
        # Add more monitors as needed
      ];
      
      # Input configuration
      input = {
        kb_layout = "us";
        follow_mouse = 1;
        touchpad = {
          natural_scroll = true;
        };
      };
      
      # General settings
      general = {
        gaps_in = 5;
        gaps_out = 10;
        border_size = 2;

        "col.active_border" = "rgba(33ccffee)";
        "col.inactive_border" = "rgba(595959aa)";
        layout = "dwindle";
      };
      
      # Decoration settings
      decoration = {
        rounding = 10;
        blur = {
          enabled = true;
          size = 3;
          passes = 1;
        };
      };
      
      # Animations
      animations = {
        enabled = true;

        animation = [
          "windows, 1, 7, default"
          "border, 1, 10, default"
          "fade, 1, 7, default"
        ];
      };
      
      # Key bindings
      bind = [
        "SUPER, Return, exec, ${pkgs.alacritty}/bin/alacritty"
        "SUPER, Q, killactive"
        "SUPER, M, exit"
        "SUPER, E, exec, ${pkgs.dolphin}/bin/dolphin"
        "SUPER, V, togglefloating"
        "SUPER, R, exec, ${pkgs.wofi}/bin/wofi --show drun"
        "SUPER, left, movefocus, l"
        "SUPER, right, movefocus, r"
        "SUPER, up, movefocus, u"
        "SUPER, down, movefocus, d"
      ];
    };
  };

  # Common programs to use with Hyprland
  programs = {
    waybar.enable = true;
    wofi.enable = true;
  };

  
  services.mako.enable = true;  # Notifications
}
