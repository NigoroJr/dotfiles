#!/bin/sh

export GTK_IM_MODULE=ibus
export GTK_IM_MODULE_FILE=/etc/gtk-2.0/x86_64-pc-linux-gnu/gtk.immodules
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

# Start ibus daemon
ibus-daemon -dx &

export LANG="en_US.UTF-8"

xrandr --output HDMI1 --primary --auto --output LVDS1 --auto --right-of HDMI1
#xrandr --output VGA1 --primary --auto --pos 0x180 --output LVDS1 --auto --pos 1440x0

imwheel -b -k "0 0 6 7 0 0 10" 1>/dev/null 2>&1

xsetroot -cursor_name left_ptr
exec xmonad
