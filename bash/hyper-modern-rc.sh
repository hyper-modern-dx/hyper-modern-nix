echo "INFO: beginning 'bash'-specific 'flox' profile initialization..."

declare USER_HOME="${HOME}"

declare HYPER_MODERN_HOME="$(realpath ${USER_HOME}/.local/share/hyper-modern)"
mkdir -p "${HYPER_MODERN_HOME}"

if [ -d "${HYPER_MODERN_HOME}" ]; then
  echo "INFO: HYPER // MODERN is installed at ${HYPER_MODERN_HOME}..."
  echo "WARNING: this feature is in development, you must currently handle the installation manually..."
elif [ -n "${HYPER_MODERN_SRC+x}" ]; then
  echo "INFO: getting HYPER // MODERN from ${HYPER_MODERN_SRC}..."
  echo "WARNING: this feature is in development, you must currently handle the installation manually..."
elif [ -n "${HYPER_MODERN_COMMIT+x}" ]; then
  echo "INFO: getting HYPER // MODERN from git commit ${HYPER_MODERN_COMMIT}..."
  echo "WARNING: this feature is in development, you must currently handle the installation manually..."
else
  echo "WARNING: HYPER // MODERN is not installed, please specify one of HYPER_MODERN_SRC or HYPER_MODERN_COMMIT for all features..."
  echo "WARNING: this feature is in development, you must currently handle the installation manually..."
fi

# default python virtual environment setup
declare DEFAULT_HYPER_MODERN_DEV_VENV_NAME="hyper-modern-dev"
declare DEFAULT_HYPER_MODERN_VENV_DIR="${HYPER_MODERN_HOME}/venvs"
declare DEFAULT_HYPER_MODERN_DEV_VENV_DIR="${DEFAULT_HYPER_MODERN_DEV_VENV_DIR}/${DEFAULT_HYPER_MODERN_DEV_VENV_NAME}"

if [ ! -d "${DEFAULT_HYPER_MODERN_DEV_VENV_DIR}" ]; then
  echo "INFO: creating default HYPER // MODERN python virtual environment at ${DEFAULT_HYPER_MODERN_DEV_VENV_DIR}..."
  python3 -mvenv "${DEFAULT_HYPER_MODERN_DEV_VENV_DIR}"
else
  echo "INFO: found default HYPER // MODERN python virtual environment at ${DEFAULT_HYPER_MODERN_DEV_VENV_DIR}."
fi

echo "INFO: activating HYPER // MODERN python virtual environment at ${DEFAULT_HYPER_MODERN_DEV_VENV_DIR}..."

source "${DEFAULT_HYPER_MODERN_DEV_VENV_DIR}/bin/activate"

echo "INFO: done activating HYPER // MODERN python virtual environment at ${DEFAULT_HYPER_MODERN_DEV_VENV_DIR}."

if [ -f "requirements.txt" ]; then
  echo "INFO: found 'requirements.txt', installing..."
  pip3 -qq install -r requirements.txt
  echo "INFO: installation complete."
fi

if [ -f "requirements_versions.txt" ]; then
  echo "INFO: found 'requirements_versions.txt', installing..."
  pip3 -qq install -r requirements_versions.txt
  echo "INFO: installation complete."
fi

# TODO(b7r7): use `flox` environment variables either within this script or in a wrapper script
# to build a `HYPER // MODERN` home directory that we can set as `$HOME` without disrupting `flox`...
# export HOME="${HYPER_MODERN_HOME}"

#
# `HYPER // MODERN` standard environment variables needing expansion
#

export LS_COLORS="$(vivid generate $VIVID_THEME)"

#
# `HYPER // MODERN` standard aliases
#

# TODO(b7r6): try to extract common flags out of substantially similar aliases
# via relatively standardized shell variable expansion...

# `HYPER // MODERN` "list" (similar to e.g. `ls -1`)
alias l="eza --icons -F -H --group-directories-first --git -1 --color=always"

# `HYPER // MODERN` list all" command (similar to e.g. `ls -ltrah`)
alias la="eza --icons -F -H --group-directories-first --git -1 --color=always --long --all"

# `HYPER // MODERN` `ls` with tree increasing tree depth, `lt`, `ltt`, `lttt`...
alias lt="eza --icons -F -H --group-directories-first --git -1 --color=always --tree --level 2"
alias ltt="eza --icons -F -H --group-directories-first --git -1 --color=always --tree --level 3"
alias lttt="eza --icons -F -H --group-directories-first --git -1 --color=always --tree --level 4"

# `lt` to "arbitrary" depth (99)
alias ltx="eza --icons -F -H --group-directories-first --git -1 --color=always --tree --level 99"

# reasonable defaults for `rg` / `bat` / `jq`

alias jq="jq -C"
alias rg="rg --color=always --colors 'match:fg:white' --colors 'path:fg:blue'"
alias bat="bat --color=always"

# quick temporary directory on the `bash` stack, great for trying `flox` environments and many things...
alias td="pushd $(mktemp -d)"

#
# `mcfly` / `zoxide` / `thefuck`
#

# TODO(b7r6): get this working reliably under e.g. `tmux` or pre-`flox activate` in `.bashrc`, one tmpclown might to
# throw the output into `[vars]` above after hand-hacking the `/nix/store` and/or `.cache/.flox/...`
# paths in, unfsck-this...
  
eval "$(mcfly init bash)"
eval "$(zoxide init bash)"
eval "$(thefuck --alias)"

#
# `starship` for prompt...
#

eval "$(starship init bash)"

echo "INFO: 'bash'-specific 'flox' profile initialization is complete."

echo "${HYPER_MODERN_ASCII_LOGO}"
