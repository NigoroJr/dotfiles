#!/bin/sh

# Update PATH so that dmenu can see stuff in ~/bin
export PATH="$HOME/bin:$PATH"

# Input Method setup
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
if hash imwheel 2>/dev/null; then
    imwheel -b -k "0 0 6 7 0 0 10" 1>/dev/null 2>&1
fi

# Set cursor to pointer
xsetroot -cursor_name left_ptr

# Disable screensaver
xset s off
xset -dpms
setterm -blank 0 -powerdown 0

# Run any local configurations (such as setting wallpaper)
if [ -f ~/.localrc/xsession ]; then
    sh ~/.localrc/xsession
fi

exec xmonad
