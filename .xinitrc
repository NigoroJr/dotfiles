#!/bin/sh

# with startx

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

xsetroot -cursor_name left_ptr
exec ck-launch-session dbus-launch xmonad
