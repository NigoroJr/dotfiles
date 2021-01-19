# Aliases

alias ls="ls --color=auto"
alias l="ls -lh"
alias la="ls -A"
alias ll="ls -CF"

alias grep="grep --color=auto"
alias egrep="egrep --color=auto"

alias df="df -h"
alias du="du -h"

alias watch="watch -n 0.5"

alias mplayer-audio="mplayer -novideo -demuxer lavf"

alias rsync="rsync --progress"

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

# Conda
alias cn="conda"
alias cni="conda install"
alias cna="conda activate"
alias cnd="conda deactivate"

# Docker
alias d="docker"
alias dr="docker run --interactive --tty"
alias dim="docker images"
alias dps="docker ps -a"
alias drm='docker ps -a | awk "/Exited/ { print \$1 }" | xargs docker rm'

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
