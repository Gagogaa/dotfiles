
# tabs still broken on git 
export EDITOR=/usr/bin/neovim																# Editor is vim
export PATH=$PATH:~/.bin																		# Add .bin to the path

shopt -s autocd																							# Auto cd for bash

alias _='sudo'																							# _ is sudo
alias edit='$EDITOR'																				# Built in edit is depricated
alias shutdown="sudo shutdown -h now"												# Turn the computer off
alias free="free -h"																				# Show free mem in a human readable form
alias ll="ls -hl"
alias l="ls -h"

# -- apt-get commands --
alias upgrade='sudo apt-get update && sudo apt-get upgrade -y && sudo apt-get dist-upgrade -y && sudo apt-get autoremove -y'
alias update='sudo apt-get update'
alias install='sudo apt-get install'
alias remove='sudo apt-get remove'
alias purge='sudo apt-get remove --purge'
alias apt-show='apt-cache show'
alias apt-search='apt-cache search'

# -- functions --
trash () { command mv "$@" ~/.local/share/Trash/files; }		# Move a file to the trash bin
cd () { builtin cd "$@"; ls; }												 			# Display contents of a dir	i cd into
mcd () { mkdir -p "$1" && cd "$1"; }												# Make a dir and jump inside of it
