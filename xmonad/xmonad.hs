

-- IMPORTS

-- base
import XMonad
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W

-- actions
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)

-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

-- data
import qualified Data.Map as M
import Data.Monoid
import Data.Ratio ((%)) -- for video

-- hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat)

-- layout
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile

-- system
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

{-- Autostart --}
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "sh -c  '~/.fehbg'"                          -- setting my wallpaper.
    spawnOnce "xsetroot -cursor_name left_ptr &"           -- setting my cursor theme.
    spawnOnce "xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &"     -- starting my compositor.

{-- Keybindings --}
myKeys = \c -> mkKeymap c $
    [ ("M-S-c",        kill)
    , ("M-C-e",        spawn "emacs")
    , ("M-S-<Return>", spawn $ terminal c)
    , ("M-C-d",        spawn "discord")
    , ("M-C-b",        spawn "firefox-bin -p")
    , ("<F6>",         spawn "xbacklight -inc 10")                      -- brightness.
    , ("<F5>",         spawn "xbacklight -dec 10")
    , ("<F3>",         spawn "amixer set Master 5%+")                   -- audio.
    , ("<F2>",         spawn "amixer set Master 5%-")
    , ("<F1>",         spawn "amixer set Master toggle")
    , ("<Print>",      spawn "gnome-screenshot -i")                     -- quit Xmonad.
    , ("M-<Space>",    sendMessage NextLayout)                             -- select layout
    , ("M-S-q",        io exitSuccess)                                  -- quit Xmonad.
    , ("M-S-r",        spawn "xmonad --recompile; xmonad --restart") ]  -- recompile and restart Xmonad.

{-- Managing window rules --}
myManageHook = composeAll
  [ className =? "notification"              --> doFloat
  , className =? "file_progress"             --> doFloat
  , className =? "toolbar"                   --> doFloat
  , className =? "confirm"                   --> doFloat
  , className =? "dialog"                    --> doFloat
  , className =? "error"                     --> doFloat
  , className =? "Gimp"                      --> doFloat
  , className =? "discord"                   --> doFloat
  , className =? "Pavucontrol"               --> doFloat
  , className =? "Slippi-r18-netplay"        --> doFloat
  , title =? "Oracle VM VirtualBox Manager"  --> doFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat ]  -- Float Firefox Dialog.

{-- Defining my workspaces --}
myWorkspaces = [ "term","emacs","www","chat","gfx","virt","misc" ]

-- layout
myLayout = avoidStruts (tiled ||| full)
    where
        -- full
        full = renamed [Replace "Full"]
                $ noBorders (Full)
        -- tiled
        tiled = renamed [Replace "Tall"]
                $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True
                $ ResizableTall 1 (3/100) (1/2) []
        -- default number of windows in master pane
        nmaster = 1
        -- default proportion of screen occupied by master pane
        ratio = 1/2
        -- percent of screen to increment by when resizing panes
        delta = 3/100

{-- All of my important stuff --}
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmobar.hs"
  xmonad $ ewmh desktopConfig
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
      , layoutHook  = smartBorders $ myLayout
      , logHook     = dynamicLogWithPP xmobarPP    -- managing pretty printing.
      { ppOutput    = hPutStrLn xmproc
      , ppVisible   = xmobarColor "#81a1c1" ""
      , ppHidden    = xmobarColor "#81a1c1" "" . wrap "*" ""
      , ppHiddenNoWindows     = xmobarColor "#4c566a" "" 
      , ppCurrent   = xmobarColor "#d8dee9" "" . wrap "[" "]"
      , ppUrgent    = xmobarColor "#bf616a" "" . wrap "!" "!"
      , ppTitle     = xmobarColor "#8fbcbb" "" . shorten 30
      , ppSep       = " | "
      , ppWsSep     = "  "
      , ppOrder     = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      }
    -- , layoutHook             = myLayout
    , terminal               = "st"                -- setting my terminal.
    , modMask                = mod4Mask            -- sebind Mod to the Windows key.
    , borderWidth            = 1                   -- border width.
    , normalBorderColor      = "#4c566a"           -- unselected border color.
    , focusedBorderColor     = "#d8dee9"           -- selected border color.
    , focusFollowsMouse      = True                -- focus follows mouse T/F.
    , workspaces             = myWorkspaces        -- defining my workspaces.
    , startupHook            = myStartupHook       -- autostarting some programs.
    , keys                   = myKeys <+> keys def -- defining mykeys for keybindings.
    } `additionalKeysP` []                         -- for those easy emacs style keybindings. 
