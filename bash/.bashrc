#    ██████╗  █████╗ ███████╗██╗  ██╗██████╗  ██████╗
#    ██╔══██╗██╔══██╗██╔════╝██║  ██║██╔══██╗██╔════╝
#    ██████╔╝███████║███████╗███████║██████╔╝██║     
#    ██╔══██╗██╔══██║╚════██║██╔══██║██╔══██╗██║     
# ██╗██████╔╝██║  ██║███████║██║  ██║██║  ██║╚██████╗
# ╚═╝╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=$PATH:~/.bin

# Shell options
# Set the default edit mode to emacs
set -o emacs

shopt -s autocd # So I can cd without typeing cd
shopt -s cdable_vars # So I can cd into variables
shopt -s cdspell # Try to fix small spelling errors
shopt -s dirspell

# A fansy propmt that looks like this
# Thinkpad|~ $>
PS1='\[$(tput setaf 2)\]\h'
PS1+='\[$(tput setaf 12)\]|'
PS1+='\[$(tput setaf 4)\]\W '
PS1+='\[$(tput setaf 12)\]\$> '
PS1+='\[$(tput sgr0)\]'
export PS1

export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim
export TRASH=~/.local/share/Trash/files

[ -f ~/.bash_aliases ]   && source ~/.bash_aliases
[ -f ~/.bash_functions ] && source ~/.bash_functions
[ -f ~/.Xresources ]     && xrdb ~/.Xresources
[ -f /etc/bashrc ]       && . /etc/bashrc # Source global definitions

# http://stackoverflow.com/questions/9652126/bashrc-profile-is-not-loaded-on-new-tmux-session-or-window-why
# [ -z $TMUX ] && tmux

# Try connecting to a tmux session on login and logout
# Don't use this until I get use to tmux
#if which tmux >/dev/null 2>&1; then
#    # if no session is started, start a new session
#    test -z ${TMUX} && tmux
#
#    # when quitting tmux, try to attach
#    while test -z ${TMUX}; do
#        tmux attach 2>/dev/null|| break
#    done
#fi
