# vim: ft=zsh

export EDITOR="vim"
if hash vimmanpager 2>/dev/null; then
    export MANPAGER=vimmanpager
fi

path=($HOME/bin(N) \
    $HOME/.rbenv/bin(N) \
    $HOME/*/adt-bundle/sdk/{tools,platform-tools}(N) \
    /usr/lib/go/bin(N) \
    /usr/local/bin(N) \
    /usr/bin(N) \
    /bin(N) \
    /usr/sbin(N) \
    /sbin(N) \
    $path)
typeset -U path

HISTFILE=~/.histfile
HISTSIZE=6000000
SAVEHIST=6000000

