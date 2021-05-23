#!/usr/bin/env bash 

xrdb -q &
killall picom
picom --daemon
feh --bg-fill $HOME/.config/qtile/wall.jpg & 
