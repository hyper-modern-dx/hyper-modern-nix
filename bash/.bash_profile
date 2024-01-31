#!/opt/homebrew/bin/bash

#
# IMPORTANT: ^ don't fuck with this...
#

if [ -f "${HOME}/.bashrc" ]; then
    source "${HOME}/.bashrc"
fi

# n.b. i want this in `.bashrc` but `brew` isn't having it, it's also fucking up
# in a way that makes me think we're getting a BSD shell, so double stupid...
#
# update, `conda` won't "init" anything but `.bash_profile`, which is of course
# some dumbfuck BSD thing on macOS. note to self: get over to `fish` or something,
# we've had it with both BSD and GNU.

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
