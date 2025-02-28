# Early exit if not running interactively
[[ $- != *i* ]] && return

# OS detection
OS="unknown"
if [[ "$OSTYPE" == "darwin"* ]]; then
	OS="macos"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
	OS="linux"
fi

# Terminal settings
export COLORTERM="truecolor"
export TERM="xterm-256color"
export EDITOR="nvim"

# Theme settings
export VIVID_THEME="nord"
export BAT_THEME="Nord"

# Function to safely add to PATH
path_append() {
	if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
		PATH="${PATH:+"$PATH:"}$1"
	fi
}

# Homebrew setup
setup_homebrew() {
	if [[ "$OS" == "macos" ]]; then
		BREW_PREFIX="/opt/homebrew"
	else
		BREW_PREFIX="/home/linuxbrew/.linuxbrew"
	fi

	if [ -d "$BREW_PREFIX" ]; then
		eval "$("$BREW_PREFIX/bin/brew" shellenv)"
		path_append "$BREW_PREFIX/bin"
	fi
}

# Path additions
declare -a USER_PATHS=(
	"${HOME}/.local/bin"
	"${HOME}/.bin"
)

for path in "${USER_PATHS[@]}"; do
	path_append "$path"
done

setup_homebrew

# Alias definitions
if command -v eza >/dev/null 2>&1; then
	export STANDARD_EZA_FLAGS="--icons -F -H --group-directories-first --git -1 --color=always"
	alias l="eza ${STANDARD_EZA_FLAGS}"
	alias la="eza ${STANDARD_EZA_FLAGS} --long --all"
	alias lt="eza ${STANDARD_EZA_FLAGS} --tree --level 2"
	alias ltt="eza ${STANDARD_EZA_FLAGS} --tree --level 3"
	alias lttt="eza ${STANDARD_EZA_FLAGS} --tree --level 4"
	alias ltx="eza ${STANDARD_EZA_FLAGS} --tree --level 99"
else
	# Fallback to standard ls with colors
	alias l="ls --color=auto"
	alias la="ls -ltrah --color=auto"
fi

# Utility aliases with existence checks
command -v jq >/dev/null 2>&1 && alias jq="jq -C"
command -v rg >/dev/null 2>&1 && alias rg="rg --color=always --colors 'match:fg:white' --colors 'path:fg:blue'"
command -v bat >/dev/null 2>&1 && alias bat="bat --color=always"

# Optional tool initializations
init_optional_tools() {
	local tool=$1
	local init_command=$2
	if command -v "$tool" >/dev/null 2>&1; then
		eval "$init_command"
	fi
}

# Load additional local configurations if they exist
for config in ~/.bashrc.d/*.sh; do
	[ -r "$config" ] && source "$config"
done

# Load uv environment if present
if [ -f "$HOME/.local/bin/env" ]; then
	source "$HOME/.local/bin/env"
fi

[[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

# TODO(b7r6): figure out why `z` freak-offs...
init_optional_tools "mcfly" "$(mcfly init bash)"
init_optional_tools "starship" "$(starship init bash)"
init_optional_tools "zoxide" "$(zoxide init bash)"

# Clean up
unset -f init_optional_tools setup_homebrew


. "$HOME/.atuin/bin/env"

[[ -f ~/.bash-preexec.sh ]] && source ~/.bash-preexec.sh
eval "$(atuin init bash)"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
