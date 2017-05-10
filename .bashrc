#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Shell options 
# Set the default edit mode to emacs
set -o emacs 

shopt -s autocd
shopt -s cdable_vars
shopt -s cdspell
shopt -s dirspell

# A fansy propmt that looks like this 
# Thinkpad|~ $> 
PS1='\[$(tput setaf 2)\]\h'
PS1+='\[$(tput setaf 12)\]|'
PS1+='\[$(tput setaf 4)\]\W '
PS1+='\[$(tput setaf 12)\]\$> '
PS1+='\[$(tput sgr0)\]'
export PS1

BROWSER=/usr/bin/chromium
EDITOR=/usr/bin/nvim
VISUAL=/usr/bin/nvim
TRASH=/.local/share/Trash/files

# See this stack overflow for file testing
# http://stackoverflow.com/questions/638975/how-do-i-tell-if-a-regular-file-does-not-exist-in-bash#638980

[ -f ~/.bash_aliases ]   && source ~/.bash_aliases
[ -f ~/.bash_functions ] && source ~/.bash_functions

# http://stackoverflow.com/questions/9652126/bashrc-profile-is-not-loaded-on-new-tmux-session-or-window-why
# [ -z $TMUX ] && tmux
# TODO: figure out a way to start the emacs server on login
# [ -e /usr/bin/emacs ] && emacs &