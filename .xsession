#!/bin/sh

export GTK_IM_MODULE=ibus
export GTK_IM_MODULE_FILE=/etc/gtk-2.0/x86_64-pc-linux-gnu/gtk.immodules
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

# Start ibus daemon
ibus-daemon -dx

LAPTOP_DISPLAY=`xrandr | grep -o 'LVDS[^ ]*'`
EXTERNAL_DISPLAY=`xrandr | grep -o 'HDMI[^ ]*'`
xrandr --output $EXTERNAL_DISPLAY --primary --auto \
    --output $LAPTOP_DISPLAY --auto \
    --right-of $EXTERNAL_DISPLAY

imwheel -b -k "0 0 6 7 0 0 10" 1>/dev/null 2>&1

# Set cursor to pointer
xsetroot -cursor_name left_ptr

# Disable screensaver
xset s off
xset -dpms
setterm -blank 0 -powerdown 0

WALLPAPER=~/Dropbox/Pictures/background.png
if [ ! -e "$WALLPAPER" ]; then
    # Use jpg file if png does not exist
    WALLPAPER=~/Dropbox/Pictures/background.jpg
fi
feh --bg-fill $WALLPAPER
# Start DropBox daemon
dropbox &

exec xmonad
