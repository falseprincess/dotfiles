
{-- Imports --}
import XMonad
import System.IO (hPutStrLn)
import XMonad.Config.Desktop
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
-- actions
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
-- data
import qualified Data.Map as M
import Data.Monoid
import Data.Ratio ((%)) -- for video
-- hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat, doRectFloat)
-- layouts and modifiers
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger

{-- Autostart --}
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "sh -c  '~/.fehbg' &"                                    -- setting my wallpaper.
    spawnOnce "xsetroot -cursor_name left_ptr &"                       -- setting my cursor theme.
    spawnOnce "xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &"                 -- starting my compositor.

{-- Keybindings --}
myKeys :: XConfig Layout -> M.Map (ButtonMask,KeySym) (X ())
myKeys = \c -> mkKeymap c $
    [ ("M-p",          spawn "dmenu_run -p 'RUN ->' -h 18")            -- launch dmenu.
    , ("M-C-f",        spawn "pcmanfm")
    , ("M-S-<Return>", spawn $ terminal c)                             -- launch my terminal.
    , ("M-C-d",        spawn "discord")
    , ("M-C-p",        spawn "pavucontrol")
    , ("M-C-b",        spawn "firefox-bin -p")
    , ("M-C-h",        spawn "st -e htop")
    , ("M-C-w",        spawn "st -e wpa_cli")
    , ("M-C-u",        spawn "st -e unimatrix")
    -- emacs.
    , ("M-C-e",        spawn "emacs")
    -- brightness.
    , ("<F6>",         spawn "xbacklight -inc 10")                     -- brightness up.
    , ("<F5>",         spawn "xbacklight -dec 10")                     -- brightness down.
    -- audio.
    , ("<F3>",         spawn "amixer set Master 5%+")                  -- audio up.
    , ("<F2>",         spawn "amixer set Master 5%-")                  -- audio down.
    , ("<F1>",         spawn "amixer set Master toggle")               -- toggle mute.
    -- screenshot.
    , ("<Print>",      spawn "gnome-screenshot -i")
    -- layouts and windows.
    , ("M-b",          sendMessage ToggleStruts)
    , ("M-<Space>",    sendMessage NextLayout)                         -- cycle through my layouts.
    , ("M-S-a",        killAll)                                        -- kill all windows in this workspace.
    , ("M-S-t",        sinkAll)                                        -- retile all floating windows.
    , ("M-S-c",        kill)                                           -- kill the focused window.
    , ("M-h",          sendMessage Shrink)                             -- Shrink horiz window width
    , ("M-l",          sendMessage Expand)                             -- Expand horiz window width
    , ("M-M1-j",       sendMessage MirrorShrink)                       -- Shrink vert window width
    , ("M-M1-k",       sendMessage MirrorExpand)                       -- Expand vert window width    
    , ("C-M1-j",       decWindowSpacing 4)                             -- decrease window spacing.
    , ("C-M1-k",       incWindowSpacing 4)                             -- increase window spacing.
    , ("C-M1-h",       decScreenSpacing 4)                             -- decrease screen spacing.
    , ("C-M1-l",       incScreenSpacing 4)                             -- increase screen spacing.
    ]
    
{-- Managing window rules --}
myManageHook = composeAll
  [ className =? "notification"              --> doFloat               -- what clients should float?
  , className =? "file_progress"             --> doFloat
  , className =? "toolbar"                   --> doFloat
  , className =? "confirm"                   --> doFloat
  , className =? "dialog"                    --> doFloat
  , className =? "error"                     --> doFloat
  , className =? "Gimp"                      --> doFloat
  , className =? "discord"                   --> doFloat
  , className =? "Pavucontrol"               --> doFloat
  , className =? "Gpick"                     --> doFloat
  , title =? "Firefox - Choose User Profile" --> doFloat
  , title =? "Oracle VM VirtualBox Manager"  --> doFloat
  
  -- pushing specified clients to said workspace upon launch.
  , title =? "Mozilla Firefox"               --> doShift ( myWorkspaces !! 2 )
  , className =? "Gimp"                      --> doShift ( myWorkspaces !! 5 )
  , className =? "discord"                   --> doShift ( myWorkspaces !! 3 )
  , className =? "VirtualBox Manager"        --> doShift ( myWorkspaces !! 4 )
  ]

{-- Defining my workspaces --}
myWorkspaces = [ "term","emacs","web","discord","vbox","gfx","media"]

{-- Layouts --}
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

tall =
  renamed [Replace "Tiled"] $ mySpacing 10 $ ResizableTall 1 (3/100) (1/2) []
  
floats =
  renamed [Replace "Float"] $ limitWindows 10 simplestFloat
  
myLayout =
  avoidStruts $ myDefaultLayout
  where
    myDefaultLayout = tall ||| smartBorders Full ||| floats

{-- All of my important stuff --}
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ ewmh desktopConfig
    -- add manage hooks while still ignoring panels and using default manageHooks
    { manageHook                = myManageHook <+> manageHook desktopConfig
      , handleEventHook         = docksEventHook
      , layoutHook              = myLayout 
      , logHook                 = dynamicLogWithPP xmobarPP           -- managing pretty printing.
      { ppOutput                = hPutStrLn xmproc
      , ppVisible               = xmobarColor "#81a1c1" ""
      , ppHidden                = xmobarColor "#8FBCBB" "" . wrap "*" ""
      , ppHiddenNoWindows       = xmobarColor "#4c566a" "" 
      , ppCurrent               = xmobarColor "#81A1C1" "" . wrap "[" "]"
      , ppUrgent                = xmobarColor "#bf616a" "" . wrap "!" "!"
      , ppTitle                 = xmobarColor "#8FBCBB" "" . shorten 40
      , ppSep                   = "<fc=#4C566A> <fn=1>|</fn> </fc>"
      , ppWsSep                 = "  "
      , ppOrder                 = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      }
    , terminal               = "st"                                   -- setting my terminal.
    , modMask                = mod4Mask                               -- sebind Mod to the Windows key.
    , borderWidth            = 1                                      -- border width.
    , normalBorderColor      = "#4C566A"                              -- unselected border color.
    , focusedBorderColor     = "#81A1C1"                              -- selected border color.
    , focusFollowsMouse      = True                                   -- focus follows mouse.
    , workspaces             = myWorkspaces                           -- defining my workspaces.
    , startupHook            = myStartupHook                          -- autostarting some programs.
    , keys                   = myKeys <+> keys def                    -- defining mykeys for keybindings.
    } `additionalKeysP`
    [ ("M-S-q", io exitSuccess)                                       -- Quits xmonad
    , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")         -- recompile and restart Xmonad.
    ] -- End of config.
    
