export PATH=$PATH:~/.bin
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx
xrdb .Xresources
