{ config
, lib
, pkgs
, ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
  init-el = builtins.readFile ./init.el;
in
{
  # Emacs configuration
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-nox;

    # Install necessary packages - combining both configurations
    extraPackages = epkgs: with epkgs; [
      all-the-icons
      all-the-icons-completion
      apheleia
      autothemer
      base16-theme
      bazel
      bind-key
      clang-format
      cmake-mode
      company
      consult
      csv-mode
      dashboard
      direnv
      dirvish
      dockerfile-mode
      doom-modeline
      eglot
      expand-region
      f
      fontify-face
      format-all
      fsharp-mode
      fzf
      general
      gptel
      haskell-mode
      hcl-mode
      ht
      iter2
      json-mode
      just-mode
      language-id
      llama
      lsp-mode
      lua-mode
      lv
      magit
      marginalia
      markdown-mode
      multiple-cursors
      mustache-mode
      nerd-icons
      nerd-icons-completion
      nix-mode
      nixpkgs-fmt
      nvm
      orderless
      org-bullets
      paredit
      paredit-everywhere
      posframe
      prettier
      prettier
      prisma-mode
      projectile
      protobuf-mode
      py-isort
      python
      rainbow-delimiters
      rainbow-mode
      reformatter
      rg
      ruff-format
      s
      shrink-path
      smartparens
      spinner
      swift-mode
      terraform-mode
      treesit-auto
      treesit-grammars.with-all-grammars
      typescript-mode
      vertico
      vterm
      wgrep
      which-key
      with-editor
      yaml-mode
      yapfify
      zig-mode
    ];

    extraConfig = init-el;
  };

  home.packages = with pkgs; [
  ];

  home.file = {
    ".emacs.d/lib".source = ./lib;
    ".emacs.d/themes".source = ./themes;
  };

  stylix.targets.emacs.enable = true;
}
