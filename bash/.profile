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
