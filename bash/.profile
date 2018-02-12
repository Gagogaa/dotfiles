[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

# This is needed for tmux to load the bashrc
source ~/.bashrc

#xrdb ~/.Xresources

# For my personal scripts
export PATH=$PATH:~/.bin
# For rust build tool cargo
export PATH=$PATH:$HOME/.cargo/bin
# For java development
export PATH=$PATH:/usr/lib/jvm/java-8-openjdk/jre/bin/

# Set the display mode
xrandr --newmode $(gtf 1920 1080 60.00 | grep Modeline | sed s/Modeline\ // | tr -d '"')
xrandr --addmode eDP-1 "1920x1080_60.00"

xrandr --newmode $(gtf 1600 900 75.00 | grep Modeline | sed s/Modeline\ // | tr -d '"')
xrandr --addmode eDP-1 "1600x900_75.00"

