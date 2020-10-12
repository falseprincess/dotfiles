# My Simple Xinitrc.

# Fix for some applications taking a long time to lanch 
dbus-update-activation-environment DISPLAY XAUTHORITY

# Setting my default refresh rate
# xrandr --refresh 120

# Merging my Xresources at launch.
xrdb -merge /home/sudozelda/.Xresources

# Launching my status bar (dwm)
slstatus &

exec dwm
# exec spectrwm
# exec qtile
# exec awesome
# exec startfluxbox
# exec twm
# exec herbstluftwm
# exec bspwm
# exec qtile
# exec awesome
# exec twm
# exec fvwm
# exec xmonad
# exec openbox
