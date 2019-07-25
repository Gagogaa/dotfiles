#!/bin/sh

# For my personal scripts
export PATH=$PATH:~/.bin

# Used for python scripts
export PATH=$PATH:~/.local/bin

# For rust packages
export PATH=$PATH:$HOME/.cargo/bin

# Adjust this for high dpi displays for qt applications
export QT_AUTO_SCREEN_SCALE_FACTOR=1

# Set this for alacritty to work under wayland
export WINIT_UNIX_BACKEND=x11

# Setup script vars
export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim
export TRASH=~/.local/share/Trash/files
