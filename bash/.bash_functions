mdprev () { 
    pandoc "$1" 1> /tmp/markdownPreview.html 2>/dev/null; 
    $BROWSER /tmp/markdownPreview.html 2>/dev/null; 
}

trash () { command mv "$@" $TRASH; }
cd () { builtin cd "$@"; ls; }
mcd () { mkdir -p "$1" && cd "$1"; }

# TODO take care of the missing arguments problem
hs () { history | grep $*; }
