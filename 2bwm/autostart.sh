#! /bin/bash

pkill polybar 
polybar mybar &

nitrogen --restore &

xsetroot -cursor_name left_ptr &

picom --config /home/sudozelda/.picom.conf --backend glx &
