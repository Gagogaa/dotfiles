#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#source .xinitrc

# Shell options 
# Set the default edit mode to emacs for now
set -o emacs 

shopt -s autocd
shopt -s cdable_vars
shopt -s cdspell
shopt -s dirspell

# propmt should look somthing like this 
# Thinkpad|~ $> 
PS1='\[$(tput setaf 2)\]\h'
PS1+='\[$(tput setaf 12)\]|'
PS1+='\[$(tput setaf 4)\]\W '
PS1+='\[$(tput setaf 12)\]\$> '
PS1+='\[$(tput sgr0)\]'
export PS1

BROWSER=/usr/bin/chromium
EDITOR=/usr/bin/nvim
TRASH=/.local/share/Trash/files
VISUAL=/usr/bin/nvim

# lean how to test if these exist before sourcing them
source .bash_aliases
source .bash_functions

