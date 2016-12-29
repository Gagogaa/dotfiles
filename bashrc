#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source .xinitrc

# Shell options 
# Set the default edit mode to emacs
set -o emacs 

shopt -s autocd
shopt -s cdable_vars

PS1='$(tput setaf 2)\h$(tput setaf 12)|$(tput setaf 4)\W $(tput setaf 12)\$> '

BROWSER=/usr/bin/chromium
EDITOR=/usr/bin/nvim
TRASH=/.local/share/Trash/files

alias l='ls -F --color=auto'
alias ls='ls -F --color=auto'
alias ll='ls -lF --color=auto'
alias la='ls -aF --color=auto'
alias lsa='ls -laF --color=auto'
alias las='ls -laF --color=auto'

alias _='sudo'
alias edit='$EDITOR'
alias free='free -h'
alias shutdown='sudo shutdown -h now'
alias h='history'
alias hsi='hs -i'
alias emacs='emacs -nw'
alias grep='grep --color=auto'
alias info='info --vi-keys'

alias update='sudo pacman -Syu'
alias install='sudo pacman -S'
alias remove='sudo pacman -Rnsc'
alias pacman='sudo pacman'

alias yupdate='yaourt -Syu'
alias yinstall='yaourt -S'
alias yremove='yaourt -Rnsc'

# Functions 
trash () { command mv "$@" $TRASH; }
cd () { builtin cd "$@"; ls; }
mcd () { mkdir -p "$1" && cd "$1"; }
hs () { history | grep $*; }

# Fuck 
#eval "$(thefuck --alias)"
#eval "$(thefuck --alias FUCK)"
