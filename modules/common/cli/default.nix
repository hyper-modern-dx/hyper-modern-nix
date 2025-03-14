{ config
, lib
, pkgs
, ...
}:
{
  imports = [
  ];

  programs.bat = {
    enable = true;
  };

  programs.eza = {
    enable = true;
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };

  # TODO[b7r6]: we should further organize this...
  home.packages = with pkgs; [
    btop
    direnv
    duf
    dust
    fd
    glow
    htop
    jq
    ripgrep
    viddy
    vivid

    nerd-fonts.hack
    xclip
    wl-clipboard
    telegram-desktop

    # TODO[b7r6]: fix ``dog`...`
    # dog
  ];
}
