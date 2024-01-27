#!/opt/homebrew/bin/bash

#
# IMPORTANT: ^ don't fuck with this...
#

#
# force "true color"
#

export COLORTERM="truecolor"
export EDITOR="nvim"
export TERM="xterm-256color"

#
# paths
#

# TODO(b7r6): find a library of basic shit like this done well and stop wasting your life...
add_to_path() {
    local new_path=$1

    # TODO(b7r6): ideally we'd be using portable stuff here...
    if [ -d "${new_path}" ] && [[ ":${PATH}:" != *":${new_path}:"* ]]; then
        export PATH="${PATH:+${PATH}:}${path}"
    fi
}

declare -ra USER_PATHS=(
    "${HOME}/.local/bin"
    "${HOME}/.bin"
    "${HOME}/bin"
    # TODO(b7r6): get this right with `uname` and shit..
    "/opt/homebrew/opt/llvm/bin"
    "/opt/homebrew/bin"
)

for path in "${USER_PATHS[@]}"; do
    add_to_path "${path}"
done

#
# aliases
#

alias l="eza --icons -F -H --group-directories-first --git -1 -X"
alias la="eza --icons -F -H --group-directories-first --git -1 --long --all"
alias lt="eza --icons -F -H --group-directories-first --git -1 --tree --level 2"
alias ltt="eza --icons -F -H --group-directories-first --git -1 --tree --level 3"
alias lttt="eza --icons -F -H --group-directories-first --git -1 --tree --level 4"
alias ltx="eza --icons -F -H --group-directories-first --git -1 --tree --level 10 --color=always"

alias jq="jq -C"
alias rg="rg --color=always --colors 'match:fg:white' --colors 'path:fg:blue'"
alias bat="bat --color=always"

alias em="open -a /Applications/Emacs.app ${1}"

#
# `vivid` / `bat` / `fzf`
#

export LS_COLORS=$(vivid generate ayu)

export BAT_THEME="Nord"

export FZF_DEFAULT_OPTS="--height 50% --layout reverse --info inline \
--border none --preview-window 50%,down,1,border-horizontal \
--ansi"

export FZF_DEFAULT_COMMAND='fd'

if [ -f ~/.fzf.bash ]; then
    source ~/.fzf.bash
fi

#
# `mcfly` / `zoxide` / `thefuck`
#

eval "$(mcfly init bash)"
eval "$(zoxide init bash)"
eval "$(thefuck --alias)"

# TODO(b7r6): do this properly...
export PATH="${PATH}:${HOME}/Library/Android/sdk/platform-tools:${HOME}/Library/Android/sdk/emulator"

# TODO(agent): do the right thing on modern darwin `brew` bash completion, i think
# this dates to a previous layout...
# if [[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]]; then
#     source "/opt/homebrew/etc/profile.d/bash_completion.sh"
# fi

# TODO(agent): this is a good example of what i don't want to do by hand...
# if [[ -r "${HOME}/bin/bazel-complete.bash" ]]; then
#     source "${HOME}/bin/bazel-complete.bash"
# fi

#
# `typescript`
#

# TODO(agent): confirm my understanding that this should just be `add_to_path` defined above v

# pnpm begin
export PNPM_HOME="/Users/b7r6/Library/pnpm"
case ":$PATH:" in
    *":$PNPM_HOME:"*) ;;
    *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

###-begin-gt-completions-###
#
# yargs command completion script
#
# Installation: gt completion >> ~/.bashrc
#    or gt completion >> ~/.bash_profile on OSX.
#
_gt_yargs_completions() {
    local cur_word args type_list

    cur_word="${COMP_WORDS[COMP_CWORD]}"
    args=("${COMP_WORDS[@]}")

    # ask yargs to generate completions.
    type_list=$(gt --get-yargs-completions "${args[@]}")

    COMPREPLY=($(compgen -W "${type_list}" -- ${cur_word}))

    # if no match was found, fall back to filename completion
    if [ ${#COMPREPLY[@]} -eq 0 ]; then
        COMPREPLY=()
    fi

    return 0
}
complete -o bashdefault -o default -F _gt_yargs_completions gt

#
# `miniconda`
#

# ...

#
# `starship` (n.b. preserve this at the end, it uses shit...)
#

eval "$(starship init bash)"
