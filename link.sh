#!/bin/sh

# This is a simple script that links the contents of the dotfiles repository
# to $HOME.

# Where all the dotfiles are
PREFIX=~/dotfiles
# Flags to add to the ln command
DEFAULT_LINK_FLAGS=-s
# Tasks to run when none specified
ALL_TASKS="vim zsh emacs X others"

while getopts 'fuh' flag; do
    case "$flag" in
        f)
            DEFAULT_LINK_FLAGS="$DEFAULT_LINK_FLAGS -f"
            ;;
        u)
            UNLINK=1
            ;;
        h)
            echo "Usage: $0 [-h] [-f] [vim|zsh|X|others]"
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

        if [[ -n $UNLINK ]] && [ $UNLINK -eq 1 ]; then
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
