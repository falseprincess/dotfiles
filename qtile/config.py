# False Princess's Qtile Config FIle.
# PLEASE READ THE DOCUMENTION BEFORE MAKING ANY EDITS TO THIS FILE!
# http://docs.qtile.org/en/latest/index.html


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                      IMPORTS 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 


import os
import re
import socket
import subprocess
from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook
from libqtile import extension
from typing import List  # noqa: F401


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                 PERSONAL VARIBLES 
# - - - - - - - - - - - - - - - - - - - - - - - - - - -


mod = "mod4" ### The Modkey.

myConfig = "/home/sudozelda/.config/qtile/config.py" ### My Default Qtile Config Location.

myTerm = "alacritty" ### My Prefered Terminal.

myEditor = "micro" ### My Prefered Text Editor.

myLocker = "betterlockscreen -l blur" ### My Prefered Lock Screen.

myFiles = "nemo" ### My Prefered File Manager.

myBrowser = "firefox-beta" ### My Prefered Browser.


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#               ESSENTIAL KEYBINDINGS 
# - - - - - - - - - - - - - - - - - - - - - - - - - - -


keys = [

    # Switch between windows in current stack pane
    Key([mod], "k", 
        lazy.layout.down()),

    Key([mod], "j", 
        lazy.layout.up()),

    # Increase/Decrease Master Window Size
    Key([mod], "l", 
        lazy.layout.grow()),
        
    Key([mod], "h", 
        lazy.layout.shrink()),

    # Put the focused window to/from floating mode
    Key([mod], "t", 
        lazy.window.toggle_floating()),
    
    # Move windows up or down in current stack
    Key([mod, "shift"], "k", 
        lazy.layout.shuffle_down()),

    Key([mod, "shift"], "j", 
        lazy.layout.shuffle_up()),

    # Open a Terminal
    Key([mod, "shift"], "Return",  
        lazy.spawn(myTerm)),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", 
        lazy.next_layout()),

    Key([mod, "shift"], "c", 
        lazy.window.kill()),

    Key([mod, "shift"], "r", 
        lazy.restart()),

    Key([mod, "shift"], "q", 
        lazy.shutdown()),

    # Run Dmenu
    Key([mod], "p", 
        lazy.spawn("dmenu_run -fn Terminus-8 -nb '#4C566A' -nf '#D8DEE9' -h 23 -sb '#A3BE8C' -sf '#2E3440'")),
    
    # Brightness keys
    Key([], "XF86MonBrightnessUp", 
        lazy.spawn("brightnessctl s +10%")),

    Key([], "XF86MonBrightnessDown", 
        lazy.spawn("brightnessctl s 10%-")),

    # Sound
    Key([], "XF86AudioMute", 
        lazy.spawn("amixer -q set Master toggle")),

    Key([], "XF86AudioLowerVolume", 
        lazy.spawn("amixer -c 0 sset Master 1- unmute")),

    Key([], "XF86AudioRaiseVolume", 
        lazy.spawn("amixer -c 0 sset Master 1+ unmute")),

    # Launch Firefox
    Key([mod, "control"], "b", 
        lazy.spawn(myBrowser)),

    # Launch Nitrogen
    Key([mod, "control"], "n",
        lazy.spawn("nitrogen")), 

    # Launch Nemo
    Key([mod, "control"], "f",
        lazy.spawn(myFiles)),

    # Launch Discord
    Key([mod, "control"], "d",
        lazy.spawn("com.discordapp.Discord")),

    # Launch Spotify
    Key([mod, "control"], "s",
        lazy.spawn("com.spotify.Client")),

    # Launch Xfce4 Taskmanager
    Key([mod, "control"], "t",
        lazy.spawn("xfce4-taskmanager")),

    # Launch Gotop
    Key([mod, "control"], "g",
        lazy.spawn("alacritty -e gotop")),

    # Lock The Screen 
    Key([mod, "control"], "2",
       lazy.spawn(myLocker)),

    # Edit My Qtile Config
    Key([mod, "control"], "1",
       lazy.spawn("alacritty -e micro ~/.config/qtile/config.py")),

    # Launch Freetube 
    Key([mod, "control"], "y",
        lazy.spawn("freetube-bin")),

    # Launch Etcher
    Key([mod, "control"], "e",
        lazy.spawn("balena-etcher-electron")),

    # Launch Pavucontrol
    Key([mod, "control"], "p",
        lazy.spawn("pavucontrol")),

    # Launch Gnome Screenshot Gui
    Key([], "Print", 
            lazy.spawn("gnome-screenshot -i")),

    # Take an instant Screenshot
    Key([mod], "Print",
            lazy.spawn("gnome-screenshot")),

    # Launch Nmtui
    Key([mod, "control"], "w",
        lazy.spawn("alacritty -e nmtui")),

    # Launch Nitrogen
    Key([mod, "control"], "n",
        lazy.spawn("nitrogen")),    
        
]


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                      GROUPS 
# - - - - - - - - - - - - - - - - - - - - - - - - - - -


group_names = [("I", {'layout': 'monadtall'}),
               ("II", {'layout': 'monadtall'}),
               ("III", {'layout': 'floating'}),
               ("IV", {'layout': 'monadtall'}),
               ("V", {'layout': 'floating'}),
               ("VI", {'layout': 'monadtall'})] ### Define Group Names, and Layout Defaults for those groups.


groups = [Group(name, **kwargs) for name, kwargs in group_names] 

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))        # Switch to another group.
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to another group.
           

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                      LAYOUTS 
# - - - - - - - - - - - - - - - - - - - - - - - - - - -


layout_theme = {"border_width": 2,
                "margin": 15,
                "border_focus": "#5E81AC",
                "border_normal": "#D8DEE9",
                "fullscreen_border_width": 2,
                } ### Set Colors And Defaults For All Layouts.

layouts = [

    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.Floating(**layout_theme),
  
]


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#             BAR/WIDGETS/MOUSE CALLBACKS 
# - - - - - - - - - - - - - - - - - - - - - - - - - - -


widget_defaults = dict(
    font='terminus',
    fontsize=13,
    padding=5,
    background="#4C566A",
    margin = 5,
)
extension_defaults = widget_defaults.copy()


def open_nmtui(qtile):
    qtile.cmd_spawn('alacritty -e nmtui')

def open_pavucontrol(qtile):
    qtile.cmd_spawn('pavucontrol')

def open_gotop(qtile):
    qtile.cmd_spawn('alacritty -e gotop')
    

screens = [
    Screen(
        top=bar.Bar(
            [
            widget.GroupBox(
                    fontsize = 13,
                    borderwidth = 2,
                    active = "#B48EAD",
                    inactive = "#81A1C1",
                    rounded = False,
                    highlight_color = "#434C5E",
                    highlight_method = "line",
                    foreground = "#81A1C1",
                    background = "#4C566A",
                    padding = 5,
                    block_highlight_text_color = '#81A1C1',
                    other_current_screen_border = '#8FBCBB',
                    other_screen_border = '#D8DEE9',
                    this_current_screen_border = '#8FBCBB',
                    urgent_border = '#BF616A',
                    margin = 3,
                    disable_drag = False,
            ),

            widget.WindowName(
                    foreground = "#A3BE8C",
                    padding = 4,
                    show_state = True,
            ),

            widget.TextBox(
                    text = '',
                    fontsize = 16,
                    foreground = "#BF616A",
                    padding = 5,
            ),

            widget.ThermalSensor(
                    foreground = "#D8DEE9",
                    metric = False,
                    update_interval = 5.0,
                    foreground_alert = "#D8DEE9",
            ), 

            widget.Sep(
                    foreground = "#D8DEE9",
                    size_percent = 60,
                    padding = 10,
            ),
                 
            widget.TextBox(
                    text = '',
                    fontsize = 16,
                    foreground = "#D08770",
                    padding = 5,
            ),

            widget.CheckUpdates(
                    distro = 'Arch',
                    execute = 'alacritty -e sudo pacman -Syu',
                    colour_have_updates = '#2E3440',
                    colour_no_updates = '#D8DEE9', 
                    update_interval = 60.0,
            ),

            widget.Sep(
                    foreground = "#D8DEE9",
                    size_percent = 60,
                    padding = 10,
            ),

            widget.TextBox(
                    text = '',
                    fontsize = 16,
                    foreground = "#EBCB8B",
                    padding = 5,
            ),
                 
            widget.Memory(
                    padding = 5,
                    foreground = "#D8DEE9",
                    update_interval = 5.0,
            ),

            widget.Sep(
                    foreground = "#D8DEE9",
                    size_percent = 60,
                    padding = 10,
            ),

            widget.TextBox(
                    text='',
                    fontsize = 35,
                    foreground = "#B48EAD",
                    padding = 5,
                    mouse_callbacks = {'Button1': open_nmtui},
            ),
                 
            widget.Net(
                    format = '{down} ↓↑ {up}',
                    padding = 5,
                    foreground = "#D8DEE9",
                    update_interval = 5.0
            ),                      

            widget.Sep(
                    foreground = "#D8DEE9",
                    size_percent = 60,
                    padding = 10,
            ),

                 widget.TextBox(
                    text = '',
                    fontsize = 35,
                    foreground = "#8FBCBB",
                    padding = 5,
            ),

            widget.Battery(
                    format = "{percent:2.0%}",
                    padding = 4,
                    foreground = "#D8DEE9",
            ),

            widget.Sep(
                    foreground = "#D8DEE9",
                    size_percent = 60,
                    padding = 10,
            ),

            
            widget.TextBox(
                    text = '',
                    fontsize = 24,
                    foreground = "#88C0D0",
                    padding = 5,
                    mouse_callbacks = {'Button1': open_gotop},
            ),
                
            widget.CPU(
                    format='{freq_current}GHz {load_percent}%',
                    foreground = "#D8DEE9",
                    padding = 5,
                    update_interval = 5.0,
            ),

            widget.Sep(
                    foreground = "#D8DEE9",
                    size_percent = 60,
                    padding = 10,
            ),

            widget.TextBox(
                    text = '',
                    fontsize = 30,
                    foreground = "#81A1C1",
                    padding = 5,
            ),

            widget.Volume(
                    foreground = "#D8DEE9",
                    padding = 5,
                    volume_app = 'pavucontrol',
            ),

            widget.Sep(
                    foreground = "#D8DEE9",
                    size_percent = 60,
                    padding = 10,
            ),

            widget.TextBox(
                    text = '',
                    fontsize = 17,
                    foreground = "#5E81AC",
                    padding = 5,
            ),
                 
            widget.Clock(format='%I:%M %p %a, %d',
                    padding = 5,
                    foreground = "#D8DEE9",
            ),

            widget.Sep(
                    foreground = "#D8DEE9",
                    size_percent = 60,
                    padding = 10,
            ),
                 
            widget.CurrentLayoutIcon(
                    scale = 0.7,
                    padding = 0,
                    custom_icon_paths=[os.path.expanduser("~/.config/qtile/icons")],
            ),

            widget.CurrentLayout(
                    padding = 5,
                    foreground = "#D8DEE9",
            ),
                
                ],
            24,
         ),
    ),
]


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#             FLOATING LAYOUT KEYBINDINGS 
# - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                   WINDOW RULES
# - - - - - - - - - - - - - - - - - - - - - - - - - - -


dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
    {'wmclass': 'discord'},
    {'wmclass': 'Nitrogen'},
    {'wmclass': 'Pavucontrol'},
    {'wmclass': 'Gnome-screenshot'},
    {'wmclass': 'Gimp'},
    {'wmclass': 'Spotify'},
    
])
auto_fullscreen = True
focus_on_window_activation = "smart"


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                 AUTOSTART SCRIPT 
# - - - - - - - - - - - - - - - - - - - - - - - - - - -


@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                   OTHER RULES
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Brings all floating windows to the front
@lazy.function
def float_to_front(qtile):
    logging.info("bring floating windows to front")
    for group in qtile.groups:
        for window in group.windows:
            if window.floating:
                window.cmd_bring_to_front()

wmname = "LG3D"
