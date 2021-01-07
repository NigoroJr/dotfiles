#!/bin/sh

# This is a simple script that links the contents of the dotfiles repository
# to $HOME. The stuff to be linked should be in a directory (and have the same
# task name) or at the top level directory. The latter is referred to as
# "others", and can be linked with the "others" task name.
#
# Examples:
#   # Link all
#   ./link.sh
#
#   # Link files and directories in the vim and emacs directory
#   ./link.sh -f vim emacs
#
#   # Unlink X11-related stuff
#   ./link.sh -u X
#
# Flags:
#   f: forces update by adding the -f switch to ln
#   u: unlinks the existing links
#   h: show help

# Where this script all the dotfiles are
PREFIX="$( cd "$( dirname "$0" )" && pwd )"
# Flags to add to the ln command
DEFAULT_LINK_FLAGS='-s -n'
# Tasks to run when none specified
ALL_TASKS="vim nvim zsh tmux emacs git X ruby python others"
UNLINK=0

while getopts 'fuh' flag; do
    case "$flag" in
        f)
            DEFAULT_LINK_FLAGS="$DEFAULT_LINK_FLAGS -f"
            ;;
        u)
            UNLINK=1
            ;;
        h)
            echo "Usage: $0 [-h] [-f] [-u] [$( echo "$ALL_TASKS" | sed -e 's/ /|/g' )]"
            exit 0
    esac

done
shift $(( $OPTIND - 1 ))

TASKS=$@
if [ $# -eq 0 ]; then
    TASKS=$ALL_TASKS
fi

process_target() {
    SRC=$1
    FLAGS="$2 $DEFAULT_LINK_FLAGS"

    for F in $PREFIX/$SRC; do
        DST=$HOME/.$( basename $SRC )

        if [ "$UNLINK" -eq 1 ]; then
            unlink $DST
        else
            ln $FLAGS $SRC $DST && \
                echo "Linked ${SRC#$PREFIX/}"
        fi
    done
}

version_required() {
    REQ="$1"
    CURRENT="$2"

    REQ_MAJOR="$( echo $REQ | cut -d. -f 1)"
    REQ_MINOR="$( echo $REQ | cut -d. -f 2)"
    REQ_PATCH="$( echo $REQ | cut -d. -f 3)"
    CURRENT_MAJOR="$( echo $CURRENT | cut -d. -f 1)"
    CURRENT_MINOR="$( echo $CURRENT | cut -d. -f 2)"
    CURRENT_PATCH="$( echo $CURRENT | cut -d. -f 3)"

    if [ "$REQ_MAJOR" -gt "$CURRENT_MAJOR" ]; then
        return 1
    elif [ "$REQ_MAJOR" -eq "$CURRENT_MAJOR" ] && \
            [ "$REQ_MINOR" -gt "$CURRENT_MINOR" ]; then
        return 1
    elif [ "$REQ_MAJOR" -eq "$CURRENT_MAJOR" ] && \
            [ "$REQ_MINOR" -eq "$CURRENT_MINOR" ] && \
            [ "$REQ_PATCH" -gt "$CURRENT_PATCH" ]; then
        return 1
    else
        return 0
    fi
}

for ARG in $TASKS; do
    echo "Running Task: $ARG"
    case "$ARG" in
        vim|zsh|emacs|X)
            for F in $PREFIX/$ARG/*; do
                process_target $F
            done
            ;;
        nvim)
            if [ "$UNLINK" -eq 1 ]; then
                unlink $HOME/.config/nvim
            else
                ln $DEFAULT_LINK_FLAGS \
                    $PREFIX/nvim \
                    $HOME/.config/nvim
            fi
            ;;
        tmux)
            if ! [ "$UNLINK" -eq 1 ] && \
                [ ! -d "$HOME/.tmux/plugins/tpm" ] && \
                hash git 2>/dev/null; then
                git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
            fi
            process_target $PREFIX/tmux/tmux.conf
            ;;
        git)
            if ! hash git >/dev/null; then
                continue
            fi

            git config --global user.name \
                "${GIT_USER_NAME:-Naoki Mizuno}"
            git config --global user.email \
                "${GIT_USER_EMAIL:-nigorojr@gmail.com}"
            git config --global core.pager \
                "${GIT_CORE_PAGER:-less -F -X}"
            git config --global merge.tool \
                "${GIT_MERGE_TOOL:-vimdiff}"
            if version_required "1.7.11" \
                "$( git --version | cut -d' ' -f 3 )"; then
                git config --global push.default \
                    "${GIT_PUSH_DEFAULT:-simple}"
            fi

            git config --global alias.a 'add'
            git config --global alias.b 'branch'
            git config --global alias.c 'commit'
            git config --global alias.ca 'commit --amend'
            git config --global alias.co 'checkout'
            git config --global alias.cob 'checkout -b'
            git config --global alias.cop 'checkout -p'
            git config --global alias.cp 'cherry-pick'
            git config --global alias.d 'diff'
            git config --global alias.dc 'diff --cached'
            git config --global alias.f 'fetch'
            git config --global alias.l 'log'
            git config --global alias.la 'log --all --graph'
            git config --global alias.lg 'log --graph'
            git config --global alias.m 'merge --no-ff'
            git config --global alias.mt 'mergetool'
            git config --global alias.pom 'push origin master'
            git config --global alias.pl 'pull'
            git config --global alias.plum 'pull upstream master'
            git config --global alias.pom 'push origin master'
            git config --global alias.r 'reset'
            git config --global alias.rem 'remote'
            git config --global alias.s 'status'
            git config --global alias.sm 'submodule'
            git config --global alias.st 'stash'
            git config --global alias.sh 'show'
            ;;
        ruby)
            if [ ! -d ~/.rbenv ]; then
                git clone https://github.com/rbenv/rbenv $HOME/.rbenv
            fi
            if [ ! -d ~/.rbenv/plugins/ruby-build ]; then
                git clone https://github.com/rbenv/ruby-build \
                    $HOME/.rbenv/plugins/ruby-build
            fi
            ;;
        python)
            PTPYTHON_DEST="$HOME/.config/ptpython/"
            if [ "$( uname -s )" = "Darwin" ]; then
                PTPYTHON_DEST="$HOME/Library/Application Support/ptpython/"
            fi

            if [ "$UNLINK" -eq 1 ]; then
                unlink "$PTPYTHON_DEST"
            else
                mkdir -p $PTPYTHON_DEST
                ln $DEFAULT_LINK_FLAGS \
                    "$PREFIX/python/config.py" \
                    "$PTPYTHON_DEST"
            fi
            ;;
        others)
            OTHER_FILES=$( find $PREFIX -maxdepth 1 -type f \
                ! -name '.git*' \
                ! -name "$( basename $0 )" )
            for F in $OTHER_FILES; do
                process_target $F
            done
            ;;
    esac
done
