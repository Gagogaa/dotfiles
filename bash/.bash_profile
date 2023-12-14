[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

# Source bashrc if running in a tmux session
if ! [ -z $TMUX ]
then
  source ~/.bashrc
fi

# For java development
export JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64"

# For rust development
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
eval "$(rustup completions bash)"
. "$HOME/.cargo/env"
