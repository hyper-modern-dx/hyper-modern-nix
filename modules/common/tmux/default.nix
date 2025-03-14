{ config
, lib
, pkgs
, ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  programs.tmux = {
    enable = true;
    prefix = "C-o";
    terminal = "tmux-256color";
    escapeTime = 10;
    historyLimit = 10000;
    keyMode = "emacs";
    baseIndex = 1;
    mouse = true;
    clock24 = true;

    extraConfig = ''
      # Use a well-supported terminal type
      set -g default-terminal "tmux-256color"

      set -ag update-environment "SSH_TTY"
      # set -s set-clipboard on
      set -g allow-passthrough on
      
      # Enable true color support for both terminals without changing terminal type
      set -ga terminal-overrides ",xterm-256color:RGB"
      set -ga terminal-overrides ",alacritty:RGB"
      set -ga terminal-overrides ",wezterm:RGB"
      set -ga terminal-overrides ",ghostty:RGB"
      
      # Key bindings from your original configuration
      bind-key C-o last-window
      
      # Pane navigation with Control-n and Control-p (from your original config)
      bind-key C-n select-pane -t :.+
      bind-key C-p select-pane -t :.-
      
      # Window navigation
      bind-key n next-window
      bind-key p previous-window
      
      # Split panes using | and -
      bind | split-window -h -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"
      unbind '"'
      unbind %
      
      # Reload configuration
      bind r source-file ~/.config/tmux/tmux.conf \; display "Reloaded!"
      
      # Status bar styling with Stylix colors
      set -g window-size latest
      
      # Message and mode styling
      set -g message-style "fg=${colors.base05},bg=${colors.base0D}"
      set -g mode-style "fg=${colors.base05},bg=${colors.base0D}"

      # Even more subtle borders using the darkest theme color
      set -g pane-border-style "fg=default"
      set -g pane-active-border-style "fg=default"

      # Status line configuration
      set -g status-interval 1
      set -g status-justify right
      
      # Left status - session name
      set -g status-left ""
      
      # Right status - date/time, host, user
      set -g status-right " #[fg=${colors.base0D}]%H:%M:%S %m-%d-%y#[default] #[fg=${colors.base0D}]#h#[default] #[fg=${colors.base0D}]#(whoami)#[default] "
      set -g status-right-length 150
      
      # Status bar styling - using base00 (darkest background color) instead of base01
      set -g status-style "fg=${colors.base03},bg=${colors.base00}"
      
      # Window status - with darker background
      set -g window-status-current-format " #[fg=${colors.base05},bg=default]#W#[default]"
      set -g window-status-format " #[fg=${colors.base03}]#W#[default] "
      set -g window-status-separator ""
    '';
  };
}
