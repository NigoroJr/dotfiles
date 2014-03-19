#!/bin/sh

# with startx

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

xsetroot -cursor_name left_ptr

xset s off
xset -dpms
setterm -blank 0 -powerdown 0

feh --bg-fill ~/Dropbox/Pictures/Wallpapers/PS/Saber\ and\ Haqua.png

exec ck-launch-session dbus-launch xmonad
