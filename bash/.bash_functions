mdprev () {
    pandoc "$1" 1> /tmp/markdownPreview.html 2>/dev/null;
    $BROWSER /tmp/markdownPreview.html 2>/dev/null;
}

trash () { command mv "$@" $TRASH; }
cd () { builtin cd "$@"; ls; }
mcd () { mkdir -p "$1" && cd "$1"; }

# TODO take care of the missing arguments problem
hs () { history | grep $*; }

fd-mount () { sudo mount -t msdos -o loop,offset=32256 /var/lib/libvirt/images/DOS/free-dos.img /mnt/Free-DOS; }
