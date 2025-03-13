{ config
, lib
, pkgs
, ...
}:
{
  # TODO[b7r6]: we should further organize this...
  home.packages = with pkgs; [
    # general development
    gh
    just
    openssl
    openssl.dev
    pkg-config
    statix
    taplo
    toml-sort
    tree-sitter
    treefmt

    # random ai stuff...
    claude-code
    (python313.withPackages
      (ps: [ ps.llm ps.llm-anthropic ]))
    
    # cloud compute
    awscli2
    google-cloud-sdk-gce
    hclfmt
    hcp
    terraform
    terragrunt
    vault-bin

    # nix
    nixd
    nixpkgs-fmt

    # python
    uv
    ruff

    # cpp
    # clang
    # clang-manpages
    # clang-analyzer
    clang-tools_19
    gcc
    gnumake

    # ruby
    rubocop
    ruby_3_1
    solargraph

    # sh
    beautysh
    shellcheck
    shfmt

    # typescript
    bun
    biome
    nodePackages_latest.nodejs
    nodePackages_latest.prettier
    nodePackages_latest.typescript-language-server
    nodePackages_latest.vscode-langservers-extracted
    nodePackages_latest.yarn
    typescript
    typescript-language-server

    # zig
    zig
    zls
  ];
}
