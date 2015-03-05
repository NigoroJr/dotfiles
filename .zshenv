# vim: ft=zsh

export LANG="en_US.UTF-8"
if [ -d $HOME/src/go ]; then
    export GOPATH=$HOME/src/go
fi

export EDITOR="vim"
if hash vimmanpager 2>/dev/null; then
    export MANPAGER=vimmanpager
fi
export ALTERNATE_EDITOR=""

path=($HOME/bin(N) \
    $HOME/usr/bin(N) \
    ${GOPATH:+$GOPATH/bin}(N) \
    $HOME/.rbenv/bin(N) \
    $HOME/.rbenv/shims(N) \
    $HOME/*/adt-bundle/sdk/{tools,platform-tools}(N) \
    /usr/lib/go/bin(N) \
    /usr/local/bin(N) \
    /usr/bin(N) \
    /bin(N) \
    /usr/sbin(N) \
    /sbin(N) \
    $path)
typeset -U path
fpath=($HOME/.zsh/functions(N) \
    $fpath)
typeset -U fpath

HISTFILE=~/.histfile
HISTSIZE=6000000
SAVEHIST=6000000
WORDCHARS="*?_-.[]~=&;!#$%^(){}<>"
