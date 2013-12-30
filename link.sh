#!/bin/sh
for F in `find ~/dotfiles/ -maxdepth 1 -name '.*' | grep -v .git`; do
    F=`basename $F`
    ln --verbose -sf ~/dotfiles/$F ~/$F
done
