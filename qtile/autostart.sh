#! /bin/bash
#
## Simple Startup Script For My Qtile Config.

# Setting my default refresh rate.
xrandr --refresh 120 &

# Merging my xresources config
xrdb -merge /home/sudozelda/.Xresources

# Starting my PolicyKit.
# /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Starting my Compositor.
picom --config /home/sudozelda/.picom.conf --backend glx  & 

# Setting my wallpaper.
nitrogen --restore &
