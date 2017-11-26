alias l='ls -F --color=auto'
alias ls='ls -F --color=auto'
alias ll='ls -lF --color=auto'
alias la='ls -aF --color=auto'
alias lla='ls -laF --color=auto'
#alias lsa='ls -laF --color=auto'
#alias las='ls -laF --color=auto'

alias _='sudo'
alias edit='$EDITOR'
alias free='free -h'
alias shutdown='sudo shutdown -h now'
alias h='history'
alias t=tmux
alias open='xdg-open $1 2>>/dev/null'

alias grep='grep --color=auto'
# TODO I might need to check and see if I have pygmentize installed.
alias ccat='pygmentize -g'
alias info='info --vi-keys'
alias copy='xclip -i'
alias past='xclip -o'

alias pacman='sudo pacman --color=auto'
alias pacaur='pacaur --color=auto'
alias rm-orphans='pacman -Rns $(pacman -Qtdq)'

# alias emacs='emacsclient --alternate-editor="" -c "$@"'
alias wg='wordgrinder'
