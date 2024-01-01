export TERM=xterm-256color
export COLORTERM=truecolor
export EDITOR="nvim"

# # colors
# declare -r CLEAR="\[\033[00m\]"
# declare -r BLACK="\[\033[0;30m\]"
# declare -r RED="\[\033[0;31m\]"
# declare -r GREEN="\[\033[0;32m\]"
# declare -r YELLOW="\[\033[0;33m\]"
# declare -r BLUE="\[\033[0;34m\]"
# declare -r MAGENTA="\[\033[0;35m\]"
# declare -r CYAN="\[\033[0;36m\]"
# declare -r WHITE="\[\033[0;37m\]"

# function set_ps1 {
#   local user="${CYAN}\u${CLEAR} "
#   local host="${YELLOW}\h${CLEAR} "
#   local dir="${BLUE}\w${CLEAR} "

#   local raw_git_branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
#   local git_branch=""
#   [ ! -z $raw_git_branch ] && git_branch="${MAGENTA}${raw_git_branch}${CLEAR} "

#   local raw_nix_ps1="${__NIX_PS1__}"
#   local nix_ps1=""
#   [ ! -z $raw_nix_ps1 ] && nix_ps1="${RED}${raw_nix_ps1}${CLEAR} "

#   local raw_conda_ps1="${CONDA_DEFAULT_ENV}"
#   local conda_ps1=""
#   [ ! -z $raw_conda_ps1 ] && conda_ps1="${RED}${raw_conda_ps1}${CLEAR} "

#   local raw_venv_ps1="$(basename ${VIRTUAL_ENV} 2>/dev/null)"
#   local venv_ps1=""
#   [ ! -z $raw_venv_ps1 ] && venv_ps1="${RED}${raw_venv_ps1}${CLEAR} "

#   export PS1="[ ${user}${host}${nix_ps1}${conda_ps1}${venv_ps1}${git_branch}${dir}] "
# }

# TODO(b7r6): figure out why we need this nonsense...
export PATH="/opt/homebrew/bin:${PATH}"
# export PATH="/opt/homebrew/opt/llvm/bin:${PATH}"

# export LS_COLORS=$(vivid generate ayu)

export BAT_THEME="Nord"

export FZF_DEFAULT_OPTS="--height 50% --layout reverse --info inline --border none --preview-window 50%,down,1,border-horizontal --ansi"

alias l="eza --icons -F -H --group-directories-first --git -1"
alias la="eza --icons -F -H --group-directories-first --git -1 --long"
alias lt="eza --icons -F -H --group-directories-first --git -1 --tree --level 2"
alias ltt="eza --icons -F -H --group-directories-first --git -1 --tree --level 3"

alias jq="jq -C"
alias rg="rg --color=always --colors 'match:fg:white' --colors 'path:fg:blue'"
alias bat="bat --color=always"

alias nai="z ~/src/natter/natter-ai"

alias gemacs="open -a /Applications/Emacs.app ${1}"
alias em="open -a /Applications/Emacs.app ${1}"

# TODO(b7r6): figure out what to do about all the different venv-types we'll need...
#
# alias python="/opt/homebrew/bin/python3"
# alias python3="/opt/homebrew/bin/python3"
# alias pip="/opt/homebrew/bin/pip3"
# alias pip3="/opt/homebrew/bin/pip3"

# add to path if exists and not present
add_to_path() {
  local new_path=$1

  if [ -d "${new_path}" ] && [[ ":${PATH}:" != *":${new_path}:"* ]]; then
    export PATH="${PATH:+${PATH}:}${path}"
  fi
}

# giving this a trial period...
add_to_path_v2 (){
    if [[ "$PATH" =~ (^|:)"${1}"(:|$) ]]
    then
        return 0
    fi
    export PATH=${1}:$PATH
}

# user paths
declare -a USER_PATHS=(
  "${HOME}/bin"
  "${HOME}/.bin"
  "${HOME}/.local/bin"
)

for path in "${USER_PATHS[@]}"; do
  add_to_path_v2 "${path}"
done

eval "$(mcfly init bash)"
eval "$(zoxide init bash)"
eval "$(thefuck --alias)"
eval "$(direnv hook bash)"

if [[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]]; then
  source "/opt/homebrew/etc/profile.d/bash_completion.sh"
fi

if [[ -r "${HOME}/bin/bazel-complete.bash" ]]; then
  source "${HOME}/bin/bazel-complete.bash"
fi

if [ -f ~/.fzf.bash ]; then
  source ~/.fzf.bash
fi

# TODO(b7r6): get this working...
# conda init "$(basename "${SHELL}")"

# TODO(b7r6): debug this...
# source $(brew --prefix nvm)/nvm.sh

# source "$HOME/.cargo/env"
export FZF_DEFAULT_COMMAND='fd'

# pnpm
export PNPM_HOME="/Users/b7r6/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH
export FZF_DEFAULT_COMMAND='fd'

eval "$(starship init bash)"

###-begin-gt-completions-###
#
# yargs command completion script
#
# Installation: gt completion >> ~/.bashrc
#    or gt completion >> ~/.bash_profile on OSX.
#
_gt_yargs_completions()
{
    local cur_word args type_list

    cur_word="${COMP_WORDS[COMP_CWORD]}"
    args=("${COMP_WORDS[@]}")

    # ask yargs to generate completions.
    type_list=$(gt --get-yargs-completions "${args[@]}")

    COMPREPLY=( $(compgen -W "${type_list}" -- ${cur_word}) )

    # if no match was found, fall back to filename completion
    if [ ${#COMPREPLY[@]} -eq 0 ]; then
      COMPREPLY=()
    fi

    return 0
}
complete -o bashdefault -o default -F _gt_yargs_completions gt
