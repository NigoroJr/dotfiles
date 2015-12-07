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
    $HOME/.pyenv/bin(N) \
    $HOME/.pyenv/shims(N) \
    $HOME/.plenv/bin(N) \
    $HOME/.plenv/shims(N) \
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
# Don't trim space after completion for these chars
ZLE_SPACE_SUFFIX_CHARS=$'|&'
# Prompt "do you wish to see all N possibilities"
# only when going out of screen
LISTMAX=0
REPORTTIME=10
TIMEFMT="\
$fg_bold[blue]%J$reset_color
  %*E :: $fg[green]U$reset_color %*U :: $fg[blue]S$reset_color %*S :: %M KiB"
