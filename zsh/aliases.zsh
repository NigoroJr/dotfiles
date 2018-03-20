#!/usr/bin/env zsh

alias ls='ls --color=auto'
alias l='ls -lh'
alias la='ls -A'
alias ll='ls -CF'

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'

alias df='df -h'
alias du='du -h'

alias watch='watch -n 0.5'

alias mplayer-audio='mplayer -novideo -demuxer lavf'

alias rsync='rsync --progress'

# Typos
alias rm_r='rm -r'
alias gS='git status'
alias gD='git diff'
alias gC='git commit'

# C++
alias gpp='g++ -std=c++11 -Wall'
alias clpp='clang++ -std=c++11'
alias clf='clang-format'

# Text Editors
alias gvi='gvim'
if (( $+commands[nvim] )); then
    alias nv='nvim'
fi
alias em='emacsclient -nw'
alias emacs='emacs -nw'
alias kill-emacs='emacsclient -e "(kill-emacs)"'
alias restart-emacs='kill-emacs && em'

alias tm='tmux'

# Git
alias g='git'
alias gai='git add -i'
alias gap='git add -p'
alias ga.='git add .'
alias gca='git commit --amend'
alias gl='git log'
alias gplum='git pull upstream master'
alias gpom='git push origin master'

alias cdd='cd $( dirname $_ )'
alias rm_='rm -f $_'

alias zmv='noglob zmv -W'
alias qv='qpdfview'

if (( $+commands[hangups] )); then
    alias hangups="hangups --key-quit='ctrl r' --key-close-tab='ctrl l' --col-palette-colors=256 --col-scheme='solarized-dark'"
fi

if (( $+commands[rosversion] )); then
    alias rgp='rosrun rqt_graph rqt_graph'
    alias rtt='rosrun rqt_tf_tree rqt_tf_tree'
    alias rsu='source devel/setup.zsh'
    alias rr='rosrun'
    alias rb='rosbag'
    alias rl='roslaunch'
    alias rte='rostopic echo'
    alias rtl='rostopic list'
    alias ck='catkin_make -DCMAKE_BUILD_TYPE=Release'
fi

# Global alias
alias -g A='| awk'
alias -g B='&& beep'
alias -g DN1='1>/dev/null'
alias -g DN12='1>/dev/null 2>&1'
alias -g DN2='2>/dev/null'
alias -g DN='>/dev/null'
alias -g EG='| egrep'
alias -g G='| grep'
alias -g GI='| grep -i'
alias -g GV='| grep -v'
alias -g H='HEAD^'
alias -g HH='HEAD^^'
alias -g HHH='HEAD^^^'
alias -g S='| sed'
alias -g SORT='| sort'
alias -g UNIQ='| uniq'
alias -g WCL='| wc -l'
if (( $+commands[fzf] )); then
    alias -g FL='| fzf'
fi

if [ -f ~/.localrc/aliases ]; then
    source ~/.localrc/aliases
fi
