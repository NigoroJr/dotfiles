#!/bin/sh

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

# Start ibus daemon
ibus-daemon -dx &

#export LANG=en_US.UTF-8

xsetroot -cursor_name left_ptr
exec xmonad
