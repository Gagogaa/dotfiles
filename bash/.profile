#!/bin/sh

# For my personal scripts
export PATH=$PATH:~/.bin

# Used for python scripts
export PATH=$PATH:~/.local/bin

# For rust packages
export PATH=$PATH:$HOME/.cargo/bin

# For ruby packages
export PATH=$PATH:$HOME/.gem/ruby/3.0.0/bin

# For flatpaks
export PATH=$PATH:/var/lib/flatpak/exports/share

# Adjust this for high dpi displays for qt applications
# export QT_AUTO_SCREEN_SCALE_FACTOR=1

# Set this for alacritty to work under wayland
# export WINIT_UNIX_BACKEND=x11

# For rust packages

# For ESP32 development
export PATH="$HOME/esp/xtensa-esp32-elf/bin:$PATH"
export IDF_PATH=~/esp/esp-idf

# For local python scripts
export PATH="$HOME/.local/bin:$PATH"

# Setup script vars
export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim
export TRASH=~/.local/share/Trash/files

# Intel Media SDK
export LIBVA_DRIVER_NAME=iHD
. "$HOME/.cargo/env"
