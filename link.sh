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

# Where all the dotfiles are
PREFIX=$HOME/dotfiles
# Flags to add to the ln command
DEFAULT_LINK_FLAGS='-s -n'
# Tasks to run when none specified
ALL_TASKS="vim nvim zsh emacs X others"

while getopts 'fuh' flag; do
    case "$flag" in
        f)
            DEFAULT_LINK_FLAGS="$DEFAULT_LINK_FLAGS -f"
            ;;
        u)
            UNLINK=1
            ;;
        h)
            echo "Usage: $0 [-h] [-f] [-u] [vim|zsh|emacs|X|others]"
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

        if [ -n "$UNLINK" ] && [ "$UNLINK" -eq 1 ]; then
            unlink $DST
        else
            ln $FLAGS $SRC $DST && \
                echo "Linked ${SRC#$PREFIX/}"
        fi
    done
}

for ARG in $TASKS; do
    echo "Running Task: $ARG"
    case "$ARG" in
        vim|zsh|emacs|X)
            for F in $PREFIX/$ARG/*; do
                process_target $F $FLAGS
            done
            ;;
        nvim)
            [ -d ~/.config/nvim ] || mkdir -p ~/.config/nvim
            # init.vim
            ln $FLAGS $DEFAULT_LINK_FLAGS \
                $PREFIX/vim/vimrc \
                $HOME/.config/nvim/init.vim
            process_target $PREFIX/vim/vim $FLAGS
            ;;
        tmux)
            if [[ ! -d ~/.tmux/plugins/tpm ]] && hash git 2>/dev/null; then
                git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
            fi
            process_target $PREFIX/tmux/*
            ;;
        others)
            OTHER_FILES=$( find $PREFIX -maxdepth 1 -type f \
                ! -name '.git*' \
                ! -name "$( basename $0 )" )
            for F in $OTHER_FILES; do
                process_target $F $FLAGS
            done
            ;;
    esac
done
