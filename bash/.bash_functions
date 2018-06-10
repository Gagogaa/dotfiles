trash () {
    command mv "$@" $TRASH;
}

cd () {
    builtin cd "$@";
    ls;
}

mcd () {
    mkdir -p "$1" && cd "$1";
}

hs () {
    history | grep $*;
}
