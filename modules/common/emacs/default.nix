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
      dashboard-hackernews
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
      json-mode
      just-mode
      language-id
      lua-mode
      magit
      marginalia
      markdown-mode
      multiple-cursors
      mustache-mode
      nerd-icons
      nerd-icons-completion
      nix-mode
      nixpkgs-fmt
      orderless
      org-bullets
      paredit
      paredit-everywhere
      posframe
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
      swift-mode
      terraform-mode
      treesit-auto
      typescript-mode
      vertico
      vterm
      wgrep
      which-key
      yaml-mode
      yapfify
      zig-mode

      llama
      hcl-mode
      lsp-mode
      ht
      lv
      spinner
      with-editor
    ];

    extraConfig = init-el;
  };

  # Add needed system packages
  home.packages = with pkgs; [
    # Essential tools
    fd
    nerd-fonts.hack
    nodejs
    ripgrep

    # Core tree-sitter (needed for grammar compilation)
    tree-sitter

    # Language servers for better IDE experience
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted # HTML/CSS/JSON/ESLint
    python311Packages.python-lsp-server
    clang-tools # For C/C++
  ];

  # Enable Stylix integration for Emacs
  stylix.targets.emacs.enable = true;
}
