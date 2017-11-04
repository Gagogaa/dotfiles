export PATH=$PATH:~/.bin
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx
xrdb ~/.Xresources
# Ubuntu make installation of Ubuntu Make binary symlink
PATH=~/.local/share/umake/bin:$PATH

# Need this for tmux to load it
source ~/.bashrc

export PATH="$HOME/.cargo/bin:$PATH"
