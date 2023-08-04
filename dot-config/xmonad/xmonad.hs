import XMonad
import System.Directory
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.FixedAspectRatio
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Gaps

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook, trayPaddingEventHook)
import XMonad.Util.NamedActions
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

import XMonad.Prompt.ConfirmPrompt

import Colors.DoomOne

-- SETUP INITIAL PARAMETERS

-- myFont :: String
myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"


-- Sets modkey to Super/Windows key
myModMask :: KeyMask
myModMask = mod4Mask


-- Sets terminal to Alacritty
myTerminal :: String
myTerminal = "alacritty"

-- Sets my default browser
myBrowser :: String
myBrowser = "firefox"

-- Sets my default text editor
myEditor :: String
myEditor = "emacs"

-- Sets border width for windows
myBorderWidth :: Dimension
myBorderWidth = 2

-- Sets boerder color for Normal windows
myNormColor :: String
myNormColor = "#282c34"

-- Sets border color of Focused windows
myFocusColor :: String
myFocusColor = "#46d9ff"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True


myWorkspaces = ["www", "terminal", "dev", "music", "slack", "game", "art", "other"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)


clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

-- Startup Hook configuration
-- ---------------------------
-- Add things to this list for startup
myStartupHook :: X ()
myStartupHook = do
  -- add a policikit startup
  spawnOnce "picom"
  spawnOnce "if [ -x /usr/bin/nm-applet ] ; then \
            \ nm-applet --sm-disable & \
            \fi"
  spawnOnce "volumeicon"
  spawnOnce "ibus-daemon -drxR"
  spawnOnce "/usr/bin/emacs --daemon"

  spawnOnce "trayer --edge top --align right --widthtype percent --transparent true --alpha 0 --tint 0x282c34 --width 10 --height 24"
  spawnOnce "xsettingsd &"
  spawnOnce "shutter -min_at_startup &"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "xfce4-power-manager"
  spawnOnce "autorandr -c"
  spawnOnce " sleep 2 && nitrogen --restore"
  setWMName "LG3D"



-- Layouts
-- ----------------------------------------
-- defining a bunch of layouts

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.


-- The layout hook
myLayoutHook = renamed [KeepWordsRight 1]
               . avoidStruts
               . fixedAspectRatio (0.5, 0.5)
               . layoutHintsWithPlacement (0.5, 0.5)
               . spacingRaw False (Border 0 0 0 0) True (Border 10 10 10 10) True -- between windows
               . gaps [(U, 15), (R, 15), (L, 15), (D, 15)] -- along the screen, excluding docks
               . mkToggle (single NBFULL) -- toggle full screen
               . smartBorders
               $ tiled
                   ||| mtiled
                   ||| full
          where
            tiled = renamed [Replace "T "] $ ResizableTall 1 (3 / 100) (5 / 8) []
            mtiled = renamed [Replace "MT"] $ Mirror tiled
            full = renamed [Replace "F "] Full


-- Manage
-- ----------------------------------------
-- Set hooks for certain types of windows
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     , className =? "re.sonny.Commit" --> doFloat
     , appName =? "pavucontrol"       --> doFloat
     , appName =?  myBrowser          --> doShift "www"
     , title   =? "MEGAsync"          --> doFloat
     , appName =? "keybase"           --> doFloat
     , title   =? "GameHub"           --> doFloat >> doShift "game"
     , title   =? "Steam"             --> doShift "game"
     , (className  =? "Steam" <&&> fmap (/= "Steam") title) --> doFloat
     , className  =? "itch"           --> doShift "game"
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     --, isFullscreen -->  doFullFloat
     ]




myKeys :: [(String, X ())]
myKeys =
  -- XMonads
   [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
   , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
   , ("M-S-q", io exitSuccess)              -- Quits xmonad
   -- Run Prompt
   , ("M-S-<Return>", spawn "dmenu_run -i -p \"Run: \"") -- Dmenu
   --, ("M-S-M1-<Return>", spawn "rofi -show drun") -- Application menu
   --, ("M-S-C-<Return>", spawn "rofi -show run") -- Program menu
   -- Useful programs to have a keybinding for launch
   , ("M-<Return>", spawn myTerminal)
   , ("M-b", spawn myBrowser)
   -- Kill windows
   , ("M-S-c", kill1)     -- Kill the currently focused client
   , ("M-S-a", killAll)   -- Kill all windows on current workspace

   -- Workspaces
   , ("M-.", nextScreen)  -- Switch focus to next monitor
   , ("M-,", prevScreen)  -- Switch focus to prev monitor
   , ("M-S-1", windows $ W.shift $ myWorkspaces !! 0)                  -- Send to workspace 1
   , ("M-S-2", windows $ W.shift $ myWorkspaces !! 1)                  -- Send to workspace 2
   , ("M-S-3", windows $ W.shift $ myWorkspaces !! 2)                  -- Send to workspace 3
   , ("M-S-4", windows $ W.shift $ myWorkspaces !! 3)                  -- Send to workspace 4
   , ("M-S-5", windows $ W.shift $ myWorkspaces !! 4)                  -- Send to workspace 5
   , ("M-S-6", windows $ W.shift $ myWorkspaces !! 5)                  -- Send to workspace 6
   , ("M-S-7", windows $ W.shift $ myWorkspaces !! 6)                  -- Send to workspace 7
   , ("M-S-8", windows $ W.shift $ myWorkspaces !! 7)                  -- Send to workspace 8
   , ("M-S-9", windows $ W.shift $ myWorkspaces !! 8)                  -- Send to workspace 9

   -- Floating windows
   , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
   , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
   , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

   -- Windows navigation
   , ("M-m", windows W.focusMaster)  -- Move focus to the master window
   , ("M-j", windows W.focusDown)    -- Move focus to the next window
   , ("M-k", windows W.focusUp)      -- Move focus to the prev window
   , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
   , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
   , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
   , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
   , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

   -- Layouts
   , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
   , ("M-<Space>", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

   -- Increase/decrease windows in the master pane or the stack
   , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase # of clients master pane
   , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
   , ("M-C-<Up>", increaseLimit)                   -- Increase # of windows
   , ("M-C-<Down>", decreaseLimit)                 -- Decrease # of windows

   -- Window resizing
   , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
   , ("M-l", sendMessage Expand)                   -- Expand horiz window width
   , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
   , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width

   -- Controls
   -- TODO: Check that keys are correctly mapped
   , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
   , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@  -1.5%")
   , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
   , ("<XF86MonBrightnessUp>", spawn "light -A 5")
   , ("<XF86MonBrightnessDown>", spawn "light -U 5")

   -- Confirm logout
   ]

main :: IO ()
main = do
  -- Launching multiple instances of xmobar on their monitors. (if you have those many)
    xmproc  <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"

    -- the xmonad, ya know...what the WM is named after!
    xmonad $ docks . ewmh $ def
      { manageHook          = myManageHook <+> manageDocks
      , handleEventHook     = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty"  <||> className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook
      , modMask             = myModMask
      , terminal            = myTerminal
      , layoutHook          = myLayoutHook
      , workspaces          = myWorkspaces
      , borderWidth         = myBorderWidth
      , normalBorderColor    = myNormColor
      , focusedBorderColor  = myFocusColor
      , logHook = dynamicLogWithPP $ xmobarPP
           -- The following variables begining with 'pp' are settings for xmobar
           { ppOutput       = \x -> hPutStrLn xmproc x                         -- xmobar on monitor 1
                              --   >> hPutStrLn xmproc1 x                        -- xmobar on monitor 2
           , ppCurrent      = xmobarColor "#98be65" "" . wrap "[" "]"          -- current workspace
           , ppVisible      = xmobarColor "#98be65" "" . clickable             -- Visible but not current workspace
           , ppHidden       = xmobarColor "#82AAFF" "" . wrap "*" "" . clickable --Hidden workspaces
           , ppHiddenNoWindows = xmobarColor "#c792ea" ""  . clickable           -- Hidden workspaces (no windows)
           , ppTitle = xmobarColor "#b3afc2" "" . shorten 90 . filter (/='\n')   -- Title of active window
           , ppSep =  "<fc=#666666> | </fc>"                                     -- Separator character
           , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"                  -- Urgent workspace
           , ppExtras  = [windowCount]                                           -- # of windows current workspace
           , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                          -- order of things in xmobar
           }
      } `additionalKeysP` myKeys
