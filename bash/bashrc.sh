#
# use flox to bring up environment
#

# declare -r FLOX="$(which flox)"

# if [ ! -x "${FLOX}" ]; then
# 	echo "'flox' is not on the path and executable, please visit https://flox.dev to install..."
# 	exit 1
# fi

#
# paths
#

# TODO(b7r6): find a library of basic shit like this done well and stop wasting your life...
add_to_path() {
	local new_path="$1"

	# TODO(b7r6): ideally we'd be using portable stuff here...
	if [ -d "${new_path}" ] && [[ ":${PATH}:" != *":${new_path}:"* ]]; then
		export PATH="${PATH:+${PATH}:}${path}"
	fi
}

declare -a USER_PATHS=(
	"${HOME}/.local/bin"
	"${HOME}/.bin"
	"${HOME}/bin"
)

for path in "${USER_PATHS[@]}"; do
	add_to_path "${path}"
done

source "${HOME}/.config/hyper-modern-rc.sh"

eval "$(mcfly init bash)"
eval "$(zoxide init bash)"
eval "$(thefuck --alias)"

eval "$(starship init bash)"
