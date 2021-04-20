#!/usr/bin/env bash

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Sourcing my xprofile.
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

# Some default x programs to start.
xsetroot -cursor_name left_ptr &
xrandr --refresh 144
xset b 0

# - - Starting my wm
# exec dbus-run-session berry

slstatus &
spotifyd --no-daemon --verbose
start-pulseaudio-x11 --daemonize=no --verbose
exec dbus-run-session dwm

# Because ufetch is retarded.
# dwm
