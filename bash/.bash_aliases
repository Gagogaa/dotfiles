alias l='ls -F --color=auto'
alias ls='ls -F --color=auto'
alias ll='ls -lF --color=auto'
alias la='ls -aF --color=auto'
alias lsa='ls -laF --color=auto'
alias las='ls -laF --color=auto'

alias _='sudo'
alias edit='$EDITOR'
alias free='free -h'
alias shutdown='sudo shutdown -h now'
alias h='history'
alias t=tmux
alias open="xdg-open $1 2>>/dev/null"
# alias hsi='hs -i'
# alias emacs='emacs -nw'
alias grep='grep --color=auto'
alias ccat='pygmentize -g'
alias info='info --vi-keys'
alias srcx='xrdb ~/.Xresources'
alias pg='ping google.com'
alias pm='pacman'
alias rm-orphans="pacman -Rns $(pacman -Qtdq)"
#alias mdprev='pandoc $1 > markdownPreview.html ; firefox markdownPreview.html'
#; firefox markdownPreview.html; rm markdownPreview.html"

# alias update='sudo pacman -Syu'
# alias install='sudo pacman -S'
# alias remove='sudo pacman -Rnsc'
alias pacman='sudo pacman --color=auto'
alias pacaur='pacaur --color=auto'

alias emacs='emacsclient --alternate-editor="" -c "$@"'
alias wg='wordgrinder'

# alias yupdate='yaourt -Syu'
# alias yinstall='yaourt -S'
# alias yremove='yaourt -Rnsc'


