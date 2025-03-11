{ config, pkgs, ... }:
let

  B = builtins;
  L = pkgs.lib;
  P = pkgs;
  U = P.callPackage ./nix/userconfigs.nix { };

  clang-tools = P.callPackage ./clang-tools { };
  pytype = P.python38Packages.callPackage ./pytype { };
  leetcode-cli = P.callPackage ./nix/leetcode-cli.nix { };
  sentence-transformers =
    P.python310Packages.callPackage ./nix/sentence-transformers.nix { };

  userPythonPackages = pypkgs:
    with pypkgs;
    [
      absl-py
      jupyterlab
      matplotlib
      numpy
      pandas
      scikit-learn
      scipy
      xgboost
      yapf

      pytorch-bin
      torchvision-bin
      transformers
      faiss
      sentence-transformers
    ] ++ [ ];

  userPython = pkgs.python310.withPackages userPythonPackages;
in {

  programs.home-manager.enable = true;

  home.packages = with P;
    [
      # difftastic
      # haskellPackages.haskell-language-server
      # haskellPackages.hindent
      # haskellPackages.hlint
      # haskellPackages.stylish-haskell
      # openjdk17_headless
      arcanist
      atop
      bat
      bazel-buildtools
      bazel_5
      bottom
      btop
      choose
      cntr
      coreutils
      curlie
      delta
      dmidecode
      dogdns
      du-dust
      duf
      ethtool
      exa
      fd
      fzf
      gdb
      gef
      gh
      git-branchless
      glances
      glow
      google-java-format
      gping
      gpu-burn
      graphviz
      hardinfo
      hstr
      htop
      hyperfine
      inetutils
      iotools
      iotop
      jq
      kmon
      lazygit
      linuxPackages.cpupower
      linuxPackages.perf
      llvmPackages_13.clang
      lsd
      lshw
      mcfly
      mob
      neovim
      ninja
      nix-index
      nixfmt
      nodePackages.node2nix
      numactl
      pciutils
      procs
      pstree
      pv
      pwndbg
      python310Packages.python-lsp-server
      ripgrep
      rnix-lsp
      rust-analyzer
      shfmt
      sloccount
      spotify-tui
      statix
      sysz
      tealdeer
      terraform
      thefuck
      tmux
      unzip
      usbutils
      userPython
      viddy
      vim
      wget
      xsv
      xterm
      zenith
      zip
      zoxide
    ] ++ [ clang-tools leetcode-cli ];

  # bash
  home.file.".bash_profile".source = ./bash/bash_profile.sh;
  home.file.".bashrc".source = ./bash/bashrc.sh;

  # vim
  home.file.".vimrc".source = ./vim/vimrc;
  home.file.".config/nvim/init.vim".source = ./vim/nvim.init.vim;

  # custom scripts
  home.file."bin/rbat".source = ./bash/rbat.sh;
  home.file."bin/remacs".source = ./bash/remacs.sh;
  home.file."bin/rvi".source = ./bash/rvi.sh;
  home.file."bin/start-ghci".source = ./bash/start-ghci.sh;

  # emacs
  #
  # TODO(armitage): figure out what `home-manager` braindamage makes this
  # necessary rather than `extraConfig` or whatever...
  home.file.".emacs.d/init.el".source = ./emacs/init.el;
  home.file.".emacs.d/lib/git-grep.el".source = ./emacs/git-grep.el;
  home.file.".emacs.d/lib/google-java-format.el".source =
    ./emacs/google-java-format.el;
  home.file.".emacs.d/lib/hs-lint.el".source = ./emacs/hs-lint.el;
  home.file.".emacs.d/lib/lark-mode.el".source = ./emacs/lark-mode.el;
  home.file.".emacs.d/lib/machine-mode.el".source = ./emacs/machine-mode.el;
  home.file.".emacs.d/lib/ripper-mode.el".source = ./emacs/ripper-mode.el;
  home.file.".emacs.d/lib/smart-split.el".source = ./emacs/smart-split.el;

  programs.emacs = {
    enable = true;
    inherit (pkgs.callPackage ./nix/emacs.nix { }) extraPackages overrides;
  };

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile ./tmux/tmux.conf;
  };

  programs.git = {
    enable = true;
    # userName = U.getFullname username;
    # userEmail = U.getEmail username;
  };
}
