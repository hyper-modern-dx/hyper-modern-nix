#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKUP_DIR="$HOME/.dotfiles.backup.$(date +%Y%m%d_%H%M%S)"
CONFIG_DIR="${HOME}/.config"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

log() {
	echo -e "${GREEN}[INSTALL]${NC} $1"
}

error() {
	echo -e "${RED}[ERROR]${NC} $1" >&2
}

backup_if_exists() {
	if [ -e "$1" ]; then
		mkdir -p "$BACKUP_DIR"
		mv "$1" "$BACKUP_DIR/"
		log "Backed up $1 to $BACKUP_DIR"
	fi
}

# Check OS
OS="unknown"
if [[ "$OSTYPE" == "darwin"* ]]; then
	OS="macos"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
	OS="linux"
fi

log "Detected OS: $OS"
log "Creating backup directory: $BACKUP_DIR"

# Install bash configurations
log "Installing bash configurations..."
backup_if_exists "$HOME/.bash_profile"
backup_if_exists "$HOME/.bashrc"
ln -sf "$SCRIPT_DIR/bash/bash_profile.sh" "$HOME/.bash_profile"
ln -sf "$SCRIPT_DIR/bash/bashrc.sh" "$HOME/.bashrc"

# Install Emacs configurations
log "Installing Emacs configurations..."
backup_if_exists "$HOME/.emacs.d"
backup_if_exists "$CONFIG_DIR/emacs"
mkdir -p "$CONFIG_DIR"
ln -sf "$SCRIPT_DIR/emacs" "$CONFIG_DIR/emacs"

# Install WezTerm configuration
log "Installing WezTerm configuration..."
backup_if_exists "$CONFIG_DIR/wezterm"
ln -sf "$SCRIPT_DIR/.config/wezterm" "$CONFIG_DIR/wezterm"

# Install Starship configuration
log "Installing Starship configuration..."
backup_if_exists "$CONFIG_DIR/starship.toml"
ln -sf "$SCRIPT_DIR/.config/starship.toml" "$CONFIG_DIR/starship.toml"

# Install Tmux configuration
log "Installing Tmux configuration..."
backup_if_exists "$HOME/.tmux.conf"
ln -sf "$SCRIPT_DIR/.tmux.conf" "$HOME/.tmux.conf"

# Set MacOS specific configurations if on MacOS
if [ "$OS" = "macos" ]; then
	log "Applying MacOS-specific settings..."
	bash "$SCRIPT_DIR/bash/macos-key-repeat-rate.sh"
fi

log "Installation complete! Please restart your terminal for changes to take effect."
if [ "$OS" = "macos" ]; then
	log "Note: MacOS key repeat rate changes require a logout/login to take effect."
fi
