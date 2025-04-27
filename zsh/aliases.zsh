# Aliases

alias pl="print -l"

alias ls="ls --color=auto"
alias l="ls -lh"
alias la="ls -A"
alias ll="ls -CF"

alias grep="grep --color=auto"
alias egrep="egrep --color=auto"

alias df="df -h"
alias du="du -h"

alias date-ymd="date +%Y%m%d"

alias watch="watch -n 0.5"

if (( $+commands[mplayer] )); then
    alias mplayer-audio="mplayer -novideo -demuxer lavf"
fi

alias rsync="rsync --progress"

if (( $+commands[pip] )); then
    alias piup="pip install --upgrade pip"
fi

# Typos
alias rm_r="rm -r"
alias gS="git status"
alias gD="git diff"
alias gC="git commit"

# C++
alias gpp="g++ -std=c++11 -Wall"
alias clpp="clang++ -std=c++11"
alias clf="clang-format"

# Text Editors
if (( $+commands[nvim] )); then
    alias v="nvim"
    alias vimdiff="nvim -d"
else
    alias v="vim"
fi
alias gvi="gvim"
alias em="emacsclient -nw"
alias emacs="emacs -nw"
alias kill-emacs='emacsclient -e "(kill-emacs)"'
alias restart-emacs="kill-emacs && em"

alias tm="tmux"

# Git
alias g="git"
alias gai="git add -i"
alias gap="git add -p"
alias ga.="git add ."
alias gca="git commit --amend"
alias gl="git log"
alias gplum="git pull upstream master"
alias gpom="git push origin master"

# Ruby
alias r="rails"
alias b="bundle"

# Docker
alias d="docker"
if (( $+commands[nvidia-docker] )); then
    alias dr="docker run --interactive --tty --gpus=all"
else
    alias dr="docker run --interactive --tty"
fi
alias drr="dr --rm"
alias dru='dr --user=$( id -u ):$( id -g )'
alias drru='drr --user=$( id -u ):$( id -g )'
alias dim="docker images"
alias dimd="docker images -f dangling=true"
alias dps="docker ps -a"
alias drm='docker ps -a | awk "/Exited/ { print \$1 }" | xargs docker rm'
alias drmi='docker images -f dangling=true | awk "!/^REPOSITORY/{ print \$3  }" | xargs -I{} docker rmi "{}"'
alias ds="docker start"
alias dbash='docker exec -it $( dps -lq ) /bin/bash'
# Compose
alias dc="docker compose"
alias dcu="docker compose up"
alias dcb="docker compose build"
alias dcr="docker compose run"
alias dcrr="docker compose run --rm"

alias cdd='cd $( dirname $_ )'
alias rm_='rm -f $_'

alias zmv="noglob zmv -W"
alias qv="qpdfview"

# Global alias
alias -g A="| awk"
alias -g DN1="1>/dev/null"
alias -g DN12="1>/dev/null 2>&1"
alias -g DN2="2>/dev/null"
alias -g DN=">/dev/null"
alias -g EG="| egrep"
alias -g G="| grep"
alias -g GI="| grep -i"
alias -g GV="| grep -v"
alias -g H="HEAD^"
alias -g HH="HEAD^^"
alias -g HHH="HEAD^^^"
alias -g S="| sed"
alias -g WCL="| wc -l"
if (( $+commands[fzy] )); then
    alias -g FL="| fzy"
elif (( $+commands[fzf] )); then
    alias -g FL="| fzf"
fi

if [ -f ~/.localrc/aliases ]; then
    source ~/.localrc/aliases
fi
