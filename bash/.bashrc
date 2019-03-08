#     888                        888
#     888                        888
#     888                        888
#     88888b.   8888b.  .d8888b  88888b.  888d888 .d8888b
#     888 "88b     "88b 88K      888 "88b 888P"  d88P"
#     888  888 .d888888 "Y8888b. 888  888 888    888
# d8b 888 d88P 888  888      X88 888  888 888    Y88b.
# Y8P 88888P"  "Y888888  88888P' 888  888 888     "Y8888P

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Shell options
# Set the default edit mode to emacs
set -o emacs

shopt -s autocd # So I can cd without typeing cd
shopt -s cdable_vars # So I can cd into variables
shopt -s cdspell # Try to fix small spelling errors
shopt -s dirspell

export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim
export TRASH=~/.local/share/Trash/files

[ -f ~/.bash_prompt ]    && source ~/.bash_prompt
[ -f ~/.bash_aliases ]   && source ~/.bash_aliases
[ -f ~/.bash_functions ] && source ~/.bash_functions
[ -f /etc/bashrc ]       && . /etc/bashrc # Source global definitions
[ -f /usr/share/bash-completion/bash_completion ] && source /usr/share/bash-completion/bash_completion
[ -f /etc/profile.d/autojump.bash ] && source /etc/profile.d/autojump.bash

# Load tmux on login and close the terminal when it exits
# [ -z $TMUX ] && tmux attach-session
