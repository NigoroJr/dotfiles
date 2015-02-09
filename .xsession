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

# Customizing mouse
if hash imwheel; then
    imwheel -b -k "0 0 6 7 0 0 10" 1>/dev/null 2>&1
fi

# Set cursor to pointer
xsetroot -cursor_name left_ptr

# Disable screensaver
xset s off
xset -dpms
setterm -blank 0 -powerdown 0

if hash dropbox; then
    dropbox &
fi

# Run any local configurations (such as setting wallpaper)
if [ -e ~/.xsession_local ]; then
    sh ~/.xsession_local
fi

# Use bumblebee if installed
if hash optirun; then
    exec optirun xmonad
else
    exec xmonad
fi
