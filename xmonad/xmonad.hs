
{-- Imports --}
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
import XMonad.Layout.Spacing
import XMonad.Layout.LimitWindows
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger

-- system
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

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
    , ("M-S-<Return>", spawn $ terminal c)                             -- launch my terminal.
    , ("M-C-d",        spawn "discord")
    , ("M-C-p",        spawn "pavucontrol")
    , ("M-C-b",        spawn "firefox-bin -p")

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

    -- layouts.
    , ("M-<Space>",    sendMessage NextLayout)                         -- cycle through my layouts.
    , ("M-S-q",        io exitSuccess)                                 -- quit Xmonad.
    , ("M-S-r",        spawn "xmonad --recompile; xmonad --restart")   -- recompile and restart Xmonad.

    -- killing windows.
    , ("M-S-a",        killAll)                                        -- kill all windows in this workspace.
    , ("M-S-t",        sinkAll)                                        -- retile all floating windows.
    , ("M-S-c",        kill)                                           -- kill the focused window.
    
    -- increase/decrease spacing (gaps)
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
  , className =? "Slippi-r18-netplay"        --> doFloat
  , title =? "Firefox - Choose User Profile" --> doFloat
  , title =? "Oracle VM VirtualBox Manager"  --> doFloat

  -- pushing specified clients to said workspace.
  , title =? "Mozilla Firefox"               --> doShift ( myWorkspaces !! 2 )
  , className =? "Gimp"                      --> doShift ( myWorkspaces !! 5 )
  , className =? "discord"                   --> doShift ( myWorkspaces !! 3 )
  , className =? "VirtualBox Manager"        --> doShift ( myWorkspaces !! 4 )
  ]

{-- Defining my workspaces --}
myWorkspaces = [ "term","emacs","www","chat","vbox","gfx","office","misc" ]

{-- Layouts --}
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

tall =
  renamed [Replace "Tall"] $ mySpacing 10 $ Tall 1 (3 / 100) (1 / 2)
full =
  renamed [Replace "Full"] $ mySpacing 0 $ limitWindows 15 Full



myLayout =
  avoidStruts $ myDefaultLayout
  where
    myDefaultLayout = tall ||| full ||| simplestFloat 

{-- All of my important stuff --}
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmobar.hs"
  xmonad $ ewmh desktopConfig
    { manageHook                = myManageHook <+> manageDocks
      , handleEventHook         = docksEventHook
      , layoutHook              = myLayout
      , logHook                 = dynamicLogWithPP xmobarPP           -- managing pretty printing.
      { ppOutput                = hPutStrLn xmproc
      , ppVisible               = xmobarColor "#81a1c1" ""
      , ppHidden                = xmobarColor "#81a1c1" "" . wrap "*" ""
      , ppHiddenNoWindows       = xmobarColor "#4c566a" "" 
      , ppCurrent               = xmobarColor "#d8dee9" "" . wrap "[" "]"
      , ppUrgent                = xmobarColor "#bf616a" "" . wrap "!" "!"
      , ppTitle                 = xmobarColor "#a3be8c" "" . shorten 40
      , ppSep                   = "<fc=#d8dee9> <fn=1>|</fn> </fc>"
      , ppWsSep                 = "  "
      , ppOrder                 = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      }
    -- , layoutHook             = myLayout
    , terminal               = "st"                                   -- setting my terminal.
    , modMask                = mod4Mask                               -- sebind Mod to the Windows key.
    , borderWidth            = 1                                      -- border width.
    , normalBorderColor      = "#4c566a"                              -- unselected border color.
    , focusedBorderColor     = "#d8dee9"                              -- selected border color.
    , focusFollowsMouse      = True                                   -- focus follows mouse.
    , workspaces             = myWorkspaces                           -- defining my workspaces.
    , startupHook            = myStartupHook                          -- autostarting some programs.
    , keys                   = myKeys <+> keys def                    -- defining mykeys for keybindings.
    } `additionalKeysP` []                        
