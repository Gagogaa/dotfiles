[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

# This is needed for tmux to load the bashrc
source ~/.bashrc

# For my personal scripts
export PATH=$PATH:~/.bin

# For java development
export PATH="/usr/lib/jvm/jdk-10.0.1/bin/:$PATH"
export JAVA_HOME="/usr/lib/jvm/jdk-10.0.1/"

# For rust development
export PATH="$PATH:$HOME/.cargo/bin"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

<<<<<<< HEAD
# For HIGH DPI displays on kde applications
export QT_AUTO_SCREEN_SCALE_FACTOR=1

=======
eval "$(rustup completions bash)"
>>>>>>> d688fc61234fd55a0da7d1ace76f838592dc0897
