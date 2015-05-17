#!/bin/sh

case `uname -s` in
    Darwin)
        # Ignore X11 files
        IGNORE='\.[xX]'
        ;;
esac

FILES=`find ~/dotfiles/ -maxdepth 1 -regextype egrep \
    -name '.*' -a \
    ! -name '.git' \
    ${IGNORE:+-a ! -regex ".*/$IGNORE.*"}`

for F in $FILES; do
    F=`basename $F`
    if [ "$F" = "." -o "$F" = ".." ]; then
        continue
    fi

    if [ "$1" = "unlink" ]; then
        unlink ~/$F
    else
        ln -vsfn ~/dotfiles/$F ~/$F
    fi
done
