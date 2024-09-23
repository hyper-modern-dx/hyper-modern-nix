#
#
#    ▬▬▬  ▬▬▬ ▬▬▬ ▬▬▬ ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬▬ ▬▬▬▬▬▬▬           ▬▬▬     ▬▬▬      ▬▬▬▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬▬ ▬▬▬▬▬▬▬  ▬▬▬▬ ▬▬▬
#    ▬▬▬  ▬▬▬ ▬▬▬ ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬      ▬▬▬  ▬▬▬         ▬▬▬     ▬▬▬       ▬▬▬ ▬▬▬ ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬      ▬▬▬  ▬▬▬ ▬▬▬▬▬▬▬▬
#    ▬▬▬▬▬▬▬▬  ▬▬▬▬▬  ▬▬▬▬▬▬▬  ▬▬▬▬▬▬   ▬▬▬▬▬▬▬         ▬▬▬     ▬▬▬        ▬▬▬ ▬▬▬ ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬▬▬▬   ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬▬
#    ▬▬▬  ▬▬▬   ▬▬▬   ▬▬▬      ▬▬▬      ▬▬▬ ▬▬▬        ▬▬▬     ▬▬▬         ▬▬▬     ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬      ▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬▬
#    ▬▬▬  ▬▬▬   ▬▬▬   ▬▬▬      ▬▬▬▬▬▬▬▬ ▬▬▬  ▬▬▬      ▬▬▬     ▬▬▬          ▬▬▬     ▬▬▬  ▬▬▬▬▬▬  ▬▬▬▬▬▬▬  ▬▬▬▬▬▬▬▬ ▬▬▬  ▬▬▬ ▬▬▬  ▬▬▬
#
#

# TODO(b7r6): the goal is for this whole file to be `flox activate -r b7r6/hyper-modern`...

#
# use flox to bring up environment
#

# declare -r FLOX="$(which flox)"

# if [ ! -x "${FLOX}" ]; then
# 	echo "'flox' is not on the path and executable, please visit https://flox.dev to install..."
# 	exit 1
# fi

# TODO(b7r6): support 16-color well...
export COLORTERM="truecolor"
export EDITOR="nvim"
export TERM="xterm-256color"

# TODO(b7r6): make this easily configurable as part of setup...
export VIVID_THEME="nord"
export BAT_THEME="Nord"

#
# `HYPER // MODERN` path handling
#

# TODO(b7r6): find a library of basic shit like this done well and stop wasting your life...
HYPER_MODERN_ADD_TO_PATH() {
	local new_path="$1"

	# TODO(b7r6): ideally we'd be using portable stuff here...
	if [ -d "${new_path}" ] && [[ ":${PATH}:" != *":${new_path}:"* ]]; then
		export PATH="${PATH:+${PATH}:}${path}"
	fi
}

# TODO(b7r6): b7r6-unfsck-this...

declare -a USER_PATHS=(
#   "${HOME}/.local/bin"
#   "${HOME}/.bin"
  "/opt/homebrew/bin"
)

for path in "${USER_PATHS[@]}"; do
  HYPER_MODERN_ADD_TO_PATH "${path}"
done

# TODO(b7r6): get here...
# source "${HOME}/.local/share/hyper-modern/hyper-modern-rc.sh"

# TODO(b7r6): pull this pattern into a utility (or better yet, get yourself a real `bash` polyfill kid)...

# TODO(b7r6): b7r6-unfsck-this...
# declare MCFLY="$(which mcfly)"
# if [ -x "${MCFLY}" ]; then
# 	echo "INFO: initializing '${MCFLY}'..."
# 	eval "$(mcfly init bash)"
# else
# 	echo "WARNING: 'mcfly' is not on the path and executable..."
# fi

# declare ZOXIDE="$(zoxide)"
# if [ -x "${ZOXIDE}" ]; then
# 	echo "INFO: initializing '${ZOXIDE}'..."
# 	eval "$(zoxide init bash)"
# else
# 	echo "WARNING: 'zoxide' is not on the path and executable..."
# fi

# declare STARSHIP="$(starship)"
# if [ -x "${STARSHIP}" ]; then
# 	echo "INFO: initializing '${STARSHIP}'..."
# 	eval "$(starship init bash)"
# else
# 	echo "WARNING: 'starship' is not on the path and executable..."
# fi

#
# `HYPER // MODERN` standard aliases
#

export STANDARD_EZA_FLAGS="--icons -F -H --group-directories-first --git -1 --color=always"

# standard hyper-modern `ls` command
alias l="eza ${STANDARD_EZA_FLAGS}"

# standard hyper-modern `ls -ltrah` or "list all" command
alias la="eza ${STANDARD_EZA_FLAGS} --long --all"

# hyper-modern `ls` with tree increasing tree depth, `lt`, `ltt`, `lttt`...
alias lt="eza ${STANDARD_EZA_FLAGS} --tree --level 2"
alias lt="eza ${STANDARD_EZA_FLAGS} --tree --level 3"
alias ltt="eza ${STANDARD_EZA_FLAGS} --tree --level 4"
alias lttt="eza ${STANDARD_EZA_FLAGS} --tree --level 5"

# list to "arbitrary" depth (99)
alias ltx="eza ${STANDARD_EZA_FLAGS} --tree --level 99"

# reasonable defaults for `rg` / `bat` / `jq`
alias jq="jq -C"
alias rg="rg --color=always --colors 'match:fg:white' --colors 'path:fg:blue'"
alias bat="bat --color=always"

#
# `mcfly` / `zoxide` / `thefuck`
#

eval "$(mcfly init bash)"
eval "$(zoxide init bash)"
eval "$(starship init bash)"

export NIXPKGS_ALLOW_UNFREE=1
export NIXPKGS_ALLOW_INSECURE=1
