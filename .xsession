#!/bin/sh

export GTK_IM_MODULE=ibus
export GTK_IM_MODULE_FILE=/etc/gtk-2.0/x86_64-pc-linux-gnu/gtk.immodules
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

# Start ibus daemon
ibus-daemon -dx &

export LANG=en_US.UTF-8

xrandr --output VGA1 --primary --auto --left-of LVDS1 1>/dev/null 2>&1

kill -9 `ps aux | grep imwheel | grep -v grep | awk '{ print $2 }'` 2>/dev/null
imwheel -b "0 0 6 7 0 0 10" 1>/dev/null 2>&1

xsetroot -cursor_name left_ptr
exec xmonad
