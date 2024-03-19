#
# terminal emulators and colors
#

# TODO(b7r6): support 16-color well...
export COLORTERM="truecolor"
export EDITOR="nvim"
export TERM="xterm-256color"

# TODO(b7r6): make this easily configurable as part of setup...
export VIVID_THEME="nord"
export BAT_THEME="Nord"

# TODO(b7r6): tune this for different screen sizes...
export FZF_DEFAULT_OPTS="--height 50% --layout reverse --info inline --border none --preview-window 50%,down,1,border-horizontal --ansi"

if [ -x "vivid" ]; then
	export LS_COLORS="$(vivid generate nord)"
fi

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
alias lttt="eza ${STANDARD_EZA_FLAGS} --tree --level 4"

# list to "arbitrary" depth (99)
alias ltx="eza ${STANDARD_EZA_FLAGS} --tree --level 99"

# reasonable defaults for `rg` / `bat` / `jq`
alias jq="jq -C"
alias rg="rg --color=always --colors 'match:fg:white' --colors 'path:fg:blue'"
alias bat="bat --color=always"

#
# `mcfly` / `zoxide` / `thefuck`
#

eval "$(mcfly init bash &>/dev/null)"
eval "$(zoxide init bash &>/dev/null)"
eval "$(thefuck --alias &>/dev/null)"

#
# `starship` for prompt...
#

eval "$(starship init bash &>/dev/null)"
