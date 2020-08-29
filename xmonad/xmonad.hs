------------------------------------------------------------------------
-- Imports -------------------------------------------------------------
------------------------------------------------------------------------

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Layout.Gaps
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ToggleHook
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.WithAll
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Setting Personal Variables ------------------------------------------
------------------------------------------------------------------------

myTerminal      = "alacritty" -- Setting my default terminal.

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True -- Whether focus follows the mouse pointer.

myClickJustFocuses :: Bool
myClickJustFocuses = False -- Whether focus follows clicking the mouse pointer.

myBorderWidth   = 2 -- Setting my default window border width.

myEditor        = "emacs" -- Setting my default editor.

myFiles         = "nemo" -- Setting my default file manager.

myBrowser       = "firefox-beta" -- Setting my default browser.

myLocker        = "betterlockscreen -l" -- Setting my default locker.

altMask = mod1Mask -- Setting my default alt key.

myModMask       = mod4Mask -- Setting my default mod key.

myPanel         = "xmobar /home/sudozelda/.xmonad/xmobar.hs &"

myFont          = "xft:Terminus:size=9:antialias=true"

myWorkspaces :: [String] 
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"] -- My workspace names.

myNormalBorderColor  = nord0
myFocusedBorderColor = nord7 -- Border colors for unfocused and focused windows, respectively.

------------------------------------------------------------------------
-- Autostart Applications. ---------------------------------------------
------------------------------------------------------------------------

myStartupHook = do
        spawnOnce "nitrogen --restore &" -- Starting my wallpaper setter.
        spawnOnce "picom --config ~/.picom.conf --backend glx &" -- Starting my compositor
        spawnOnce "xsetroot -cursor_name left_ptr" -- Setting my default mouse theme.
        spawnOnce "xrandr --refresh 120" -- Setting my default refresh rate.

------------------------------------------------------------------------
-- My Nord Colors. -----------------------------------------------------
------------------------------------------------------------------------

nord0 = "#2E3440" -- Polar night
nord1 = "#3B4252" -- Brighter polar night
nord2 = "#434C5E" -- Even brighter polar night
nord3 = "#4C566A" -- Brightest polar night
nord4 = "#D8DEE9" -- Snow storm
nord5 = "#E5E9F0" -- Brighter snowstorm
nord6 = "#ECEFF4" -- Brightest snowstorm
nord7 = "#8FBCBB" -- Frost teal
nord8 = "#88C0D0" -- Frost Cyan
nord9 = "#81A1C1" -- Frost light blue
nord10 = "#5E81AC" -- Frost deep blue
nord11 = "#BF616A" -- Aurora red
nord12 = "#D08770" -- Aurora orange
nord13 = "#EBCB8B" -- Aurora yellow
nord14 = "#A3BE8C" -- Aurora green
nord15 = "#B48EAD" -- Aurora puprle

------------------------------------------------------------------------
-- Custom Window Rules -------------------------------------------------
------------------------------------------------------------------------

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "discord"           --> doFloat
    , className =? "Pavucontrol"           --> doFloat
    , className =? "Gpick"           --> doFloat
    , className =? "SimpleScreenRecorder"           --> doFloat
    , className =? "Nitrogen"           --> doFloat
    , className =? "Galculator"           --> doFloat
    , className =? "Gnome-screenshot"           --> doFloat
    , className =? "Xarchiver"           --> doFloat
    
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , toggleHook "float" doFloat <+> manageHook def

    ]  
   
------------------------------------------------------------------------
-- Keybindings --------------------------------------------------------
------------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- Terminal hotkeys                                           
    [ ((modm .|. shiftMask, xK_Return), spawn (myTerminal))

    , ((modm .|. controlMask, xK_g   ), spawn (myTerminal ++ " -e gotop"))

    , ((modm .|. controlMask, xK_w   ), spawn (myTerminal ++ " -e nmtui"))

    , ((modm .|. controlMask, xK_u   ), spawn (myTerminal ++ " -e unimatrix -c green"))

    -- Lock the screen.
    , ((modm .|. shiftMask, xK_Delete   ), spawn (myLocker))

    -- Brightness keys.
    , ((0, xF86XK_MonBrightnessUp    ), spawn "brightnessctl s +2%") -- Increase brightness.

    , ((0, xF86XK_MonBrightnessDown  ), spawn "brightnessctl s 2%-") -- Decrease brightness.


    -- Volume/Mic keys
    , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer -D pulse sset Master '2%-'") -- Decrease volume.

    , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer -D pulse sset Master '2%+'") -- Increase volume.

    , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle") -- Mute Volume.
        
    -- Xmonad 
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restarts and recompiles Xmonad.
    
    , ((modm .|. controlMask, xK_3   ), spawn "systemctl reboot") -- Restarts linux.

    , ((modm .|. controlMask, xK_4   ), spawn "systemctl poweroff") -- Powers off linux.

    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- Logout of Xmonad.

    -- Emacs hotkeys
    , ((modm .|. controlMask, xK_e   ), spawn (myEditor))

    , ((modm .|. controlMask, xK_1   ), spawn (myEditor ++ " /home/sudozelda/.xmonad/xmonad.hs")) -- Open my Xmonad config
    
    , ((modm .|. controlMask, xK_2   ), spawn (myEditor ++ " /home/sudozelda/.xmonad/xmobarrc")) -- Open my xmobarrc
      
    -- Personal gui application hotkeys
    , ((modm .|. controlMask, xK_p   ), spawn "pavucontrol")
    
    , ((modm .|. controlMask, xK_b   ), spawn (myBrowser))

    , ((modm,   xK_p     ), spawn "dmenu_run  -fn Terminus-9 -h 25  -nb '#2E3440' -nf '#D8DEEA' -sb '#88C0D0' -sf '#2E3440'")
    
    , ((modm .|. controlMask, xK_f   ), spawn (myFiles))

    , ((modm .|. controlMask, xK_d   ), spawn "discord-ptb")

    , ((modm .|. controlMask, xK_n   ), spawn "nitrogen")
    
    -- Close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Kill all windows in the current workspace -- The Window Nuker 
    , ((modm .|. shiftMask, xK_x   ), killAll)

    -- Float all windows
    , ((modm .|. shiftMask, xK_t), sinkAll)

    -- Screenshot tool
    , ((0, xK_Print                 ), spawn "gnome-screenshot -i")

    -- Instant Screenshot
    , ((modm, xK_Print              ), spawn "gnome-screenshot")

    -- Launch my screen recorder
    , ((modm, xK_s                  ), spawn "simplescreenrecorder")
      
    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle struts
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    ]
    ++
    
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
    
------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
     where 
  
     -- default tiling algorithm partitions the screen into two panes
     tiled   =  spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc <- spawnPipe "xmobar /home/sudozelda/.xmonad/xmobar.hs &"
    xmonad $ docks defaultConfig
        { layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

  
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
