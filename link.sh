#!/bin/sh

case `uname -s` in
    Darwin)
        # Ignore X11 files
        IGNORE='\.[xX]'
        ;;
esac

if [[ -n $IGNORE ]]; then
    FILES=`find ~/dotfiles/ -maxdepth 1 -name '.*' \
        | grep -v '\.git' \
        | grep -v "$IGNORE"`
else
    FILES=`find ~/dotfiles/ -maxdepth 1 -name '.*' \
        | grep -v '\.git'`
fi

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
