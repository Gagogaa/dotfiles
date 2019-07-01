[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

source ~/.bashrc

# For my personal scripts
export PATH=$PATH:~/.bin
export PATH=$PATH:~/.local/bin

# For rust development
export PATH="$PATH:$HOME/.cargo/bin"
if (which rustup >> /dev/null); then
  eval "$(rustup completions bash)"
fi

# Adjust this for high dpi displays for qt applications
export QT_AUTO_SCREEN_SCALE_FACTOR=1

# Set this for alacritty to work under wayland
export WINIT_UNIX_BACKEND=x11
