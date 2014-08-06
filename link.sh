#!/bin/sh

for F in `find ~/dotfiles/ -maxdepth 1 -name '.*' | grep -v '\.git'`; do
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
