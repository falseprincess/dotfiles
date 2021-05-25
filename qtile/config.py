# ----------------------------------------------------------------------------
# Imports.
import os
import re
import socket
import subprocess

from typing import List
from libqtile import qtile
from libqtile import extension
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook
from libqtile.config import Key, Screen, Group, Drag, Click

# ----------------------------------------------------------------------------
# Hooks.
@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home]) #  ^autostarts defined script^ 

# ----------------------------------------------------------------------------
# Defintions.
term = "st"
alt = "mod1"
mod = "mod4"
wmname = "LG3D"
editor = "emacs"

# ----------------------------------------------------------------------------
# Gruvbox Colors.
bg = "#2e3440"
fg = "#d8dee9"
fg_alt = "#4c566a"
red = "#bf616a"
green = "#a3be8c"
cyan = "#88c0d0"
yellow = "#ebcb8b"
blue = "#5e81ac"
blue1 = "#81a1c1"
purple = "#b48ead"
orange = "#d08770"

# ----------------------------------------------------------------------------
# Keybindings.
keys = [

    # MonadTall keys.
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    
    Key([mod], "i", lazy.layout.grow()),
    Key([mod], "m", lazy.layout.shrink()),
    Key([mod], "o", lazy.layout.maximize()),
    Key([mod], "n", lazy.layout.normalize()),
    Key([mod, "shift"], "space", lazy.layout.flip()),

    # my menu/run prompt.
    Key(['mod4'], 'p', lazy.run_extension(extension.DmenuRun(
        dmenu_prompt="RUN ->",
        dmenu_font="GohuFont",
        background=bg,
        foreground=fg,
        selected_background=blue,
        selected_foreground=bg,
        dmenu_ignorecase=False,
        dmenu_height=21, 
    ))),
    
    # launch my terminal.
    Key([mod, "shift"], "Return", lazy.spawn(term)),

    # launch my editor.
    Key([mod, "control"], "e", lazy.spawn(editor)),

    # launch normiecord.
    Key([mod, "control"], "d", lazy.spawn("/usr/bin/discord --no-sandbox")),

    # launch pcmanfm.
    Key([mod, "control"], "f", lazy.spawn("pcmanfm")),

    # launch my browser.
    Key([mod, "control"], "b", lazy.spawn("firefox-bin")),

    # brightness.
    Key([], "F6", lazy.spawn("xbacklight -inc 10+")),
    Key([], "F5", lazy.spawn("xbacklight -dec 10-")),

    # volume.
    Key([], "F3", lazy.spawn("amixer set Master 5%+")),
    Key([], "F2", lazy.spawn("amixer set Master 5%-")),
    Key([], "F1", lazy.spawn("amixer set Master toggle")),
    
    # screenshot.
    Key([], "Print", lazy.spawn("gnome-screenshot -i")),

    # qtile general keys.
    Key([mod, "shift"], "c", lazy.window.kill()),
    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod, "shift"], "q", lazy.shutdown()),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

# - - - Floating window rules.
dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
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
    {'wmclass': 'kcolor-picker'},
    {'wmclass': 'galculator'},
    {'wmclass': 'lxappearance'},
    {'wmclass': 'gnome-screenshot'},
], border_width = 2, border_focus = fg, border_normal = fg_alt)
auto_fullscreen = True
focus_on_window_activation = "smart"

# ----------------------------------------------------------------------------
# Groups.
group_names = [("TERM", {'layout': 'monadtall'}),
               ("DEV", {'layout': 'monadtall'}),
               ("WEB", {'layout': 'monadtall'}),
               ("CHAT", {'layout': 'monadtall'}),
               ("MEDIA", {'layout': 'monadtall'}),
               ("GFX", {'layout': 'monadtall'}),
               ("VM", {'layout': 'monadtall'}),
               ("MISC", {'layout': 'monadtall'})]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name)))

# ----------------------------------------------------------------------------
# Layouts.
layouts = [
    layout.MonadTall(
        border_focus=fg,
        border_normal=fg_alt,
        border_width=2,
        change_size=20,
        margin=10,
    ),
]

# ----------------------------------------------------------------------------
# Panel and Widgets.
widget_defaults = dict(
    font='GohuFont',
    fontsize=14,
    padding=3,
    background = bg,
    foreground = fg,  
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
               #  widget.Image(
               #      filename='~/.config/qtile/gentoo.png',
               #      scale=True
               #  ),
                widget.GroupBox(
                    disable_drag=True,
                    use_mouse_wheel=False,
                    center_aligned=True,
                    rounded=False,
                    borderwidth=0,
                    highlight_method='line',
                    highlight_color=[blue, blue1],
                    block_highlight_text_color=bg,
                    this_current_screen_border=blue1,
                    this_screen_border=blue1,
                    active=fg,
                    inactive=fg_alt,
                    padding=8
                    
                ),
                widget.WindowName(
                    background=bg,
                    foreground=purple,
                    format='{state}{name}',
                    max_chars=20,
                    padding=10
                ),

                widget.TextBox(
                    text='{',
                    foreground=fg_alt
                ),
                
                widget.Net(
                    padding=5,
                    foreground=blue1,
                    format='{down} ↓↑ {up}',
                    update_interval=1
                ),

                widget.TextBox(
                    text='|',
                    padding=5,
                    foreground=fg_alt
                ),

                widget.TextBox(
                    text='',
                    padding=5,
                    fontsize=14,
                    foreground=green
                ),

                widget.CPU(
                    foreground=green,
                    update_interval=5.0,
                    format='CPU {freq_current}GHz {load_percent}%'  
                ),

                widget.TextBox(
                    text='|',
                    padding=5,
                    foreground=fg_alt
                ),
                
                 widget.TextBox(
                    text='',
                    padding=5,
                    fontsize=16,
                    foreground=blue1
                ),
                widget.Volume(
                    padding=5,
                    foreground=blue1,
                    device='default',
                    update_interval=0.1
                ),

                widget.TextBox(
                    text='|',
                    padding=5,
                    foreground=fg_alt
                ),
                
                widget.TextBox(
                    text='',
                    padding=5,
                    fontsize=14,
                    foreground=green
                ),
                widget.Battery(
                    padding=5,
                    foreground=green,
                    empty_char='x',
                    discharge_char='V',
                    charge_char='^',
                    format='{percent:2.0%}',
                    update_interval=60
                ),

                widget.TextBox(
                    text='|',
                    padding=5,
                    foreground=fg_alt
                ),
                
                widget.Clock(
                    padding=5,
                    format='%a %I:%M %p',
                    foreground=blue1,
                    update_interval=60
                ),

                widget.TextBox(
                    text='}',
                    padding=5,
                    foreground=fg_alt
                ),
            ],
            25,
        ),
    ),
]
