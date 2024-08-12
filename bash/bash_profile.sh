if [ -f "${HOME}/.bashrc" ]; then
  source "${HOME}/.bashrc"
fi

# Added by OrbStack: command-line tools and integration
# source ~/.orbstack/shell/init.bash 2>/dev/null || :
export PATH="/home/linuxbrew/.linuxbrew/opt/clang-format/bin:$PATH"
