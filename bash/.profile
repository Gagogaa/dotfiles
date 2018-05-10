[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

# This is needed for tmux to load the bashrc
source ~/.bashrc

# xrdb ~/.Xresources

# For my personal scripts
# export PATH=$PATH:~/.bin
# For rust build tool cargo
export PATH="$PATH:$HOME/.cargo/bin"
# For java development
# export PATH="$PATH:/usr/lib/jvm/java-1.8.0-openjdk/bin/"
# export PATH="/usr/lib/jvm/java-9-openjdk-9.0.4.11-4.fc27.x86_64/bin/"
# export JAVA_HOME="/usr/lib/jvm/java-1.8.0-openjdk"
# export JAVA_HOME="/usr/lib/jvm/java-9-openjdk-9.0.4.11-4.fc27.x86_64/"

# Set better screen scaling
# gsettings set org.gnome.desktop.interface scaling-factor 2

# Setting for ulauncher
# export GDK_BACKEND=x11

# Set the display mode
#xrandr --newmode $(gtf 1920 1080 60.00 | grep Modeline | sed s/Modeline\ // | tr -d '"')
#xrandr --addmode eDP-1 "1920x1080_60.00"

#xrandr --newmode $(gtf 1600 900 75.00 | grep Modeline | sed s/Modeline\ // | tr -d '"')
#xrandr --addmode eDP-1 "1600x900_75.00"


