```txt
 ▬▬▬  ▬▬▬ ▬▬▬ ▬▬▬ ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬▬ ▬▬▬▬▬▬▬           ▬▬▬     ▬▬▬      ▬▬▬▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬▬ ▬▬▬▬▬▬▬  ▬▬▬▬ ▬▬▬
 ▬▬▬  ▬▬▬ ▬▬▬ ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬      ▬▬▬  ▬▬▬         ▬▬▬     ▬▬▬       ▬▬▬ ▬▬▬ ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬      ▬▬▬  ▬▬▬ ▬▬▬▬▬▬▬▬
 ▬▬▬▬▬▬▬▬  ▬▬▬▬▬  ▬▬▬▬▬▬▬  ▬▬▬▬▬▬   ▬▬▬▬▬▬▬         ▬▬▬     ▬▬▬        ▬▬▬ ▬▬▬ ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬▬▬▬   ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬▬
 ▬▬▬  ▬▬▬   ▬▬▬   ▬▬▬      ▬▬▬      ▬▬▬ ▬▬▬        ▬▬▬     ▬▬▬         ▬▬▬     ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬      ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬▬
 ▬▬▬  ▬▬▬   ▬▬▬   ▬▬▬      ▬▬▬▬▬▬▬▬ ▬▬▬  ▬▬▬      ▬▬▬     ▬▬▬          ▬▬▬     ▬▬▬  ▬▬▬▬▬▬  ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬  ▬▬▬
```

# Multi-Platform NixOS Configuration

This repository contains a modern, declarative system configuration using Nix, Home Manager, and Stylix for consistent theming. It supports multiple platforms (aarch64/x86_64) and operating systems (NixOS/Darwin).

## Features

- **Modular Structure** - Configuration divided into shared modules and host-specific settings
- **Cross-Platform** - Support for both Linux (NixOS) and macOS (Darwin)
- **Declarative UI** - Consistent theming with Stylix across all applications
- **Developer-Focused** - First-class support for Python (Astral tools), Nix, and more
- **Modern Terminals** - Configuration for ghostty and wezterm with true-color support

## Quick Start

To build a specific system:

```bash
# Initial setup
sudo nixos-rebuild switch --flake .#watchtower

# Update dependencies and rebuild
nix flake update
sudo nixos-rebuild switch --flake .#watchtower
```

## Structure

- **modules/** - Shared configuration modules
  - **common/** - Cross-platform shared settings
  - **nixos/** - NixOS-specific modules
  - **darwin/** - macOS-specific modules
  - **home/** - Home-manager specific modules
- **hosts/** - Host-specific configurations
  - **watchtower/** - Example aarch64-linux VM
- **legacy/** - Reference configurations from previous setup

## Development

For development environments:

```bash
# Development shell with useful tools
nix develop

# Format code
treefmt
```

See [CLAUDE.md](./CLAUDE.md) for additional development guidelines and standards.