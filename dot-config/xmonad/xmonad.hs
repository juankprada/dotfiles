  -- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
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
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar.PP (filterOutWsPP)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- SETUP INITIAL PARAMETERS

-- myFont :: String

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
  spawnOnce "ibus-daemon -drxR"
  spawnOnce "xrandr --output eDP --left-of DP-1-2 --output DP-1-2 --primary --left-of HDMI-1-0 --output HDMI-1-0 --auto"
  spawnOnce "trayer --edge top --align right --widthtype percent --transparent true --alpha 0 --tint 0x282c34 --width 10 --height 24"
  spawnOnce "xsettingsd &"
  spawnOnce "picom"
  spawnOnce "shutter -min_at_startup &"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "xfce4-power-manager"
  spawnOnce "if [ -x /usr/bin/nm-applet ] ; then \
            \ nm-applet --sm-disable & \
            \fi"
  spawnOnce "nitrogen --restore"
  setWMName "LG3D"





myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1        -- default number of windows in the master pane
    ratio    = 1/2      -- default proportion of screen occupied by master pane
    delta    = 3/100    -- Percent pf screen tp increment by when resizing panes

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
                , NS "inputHelper" spawnInput findInput manageInput
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "gnome-calculator"
    findCalc   = className =? "gnome-calculator"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.2
                 t = 0.75 -h
                 l = 0.60 -w
    spawnInput  = "~/bin/steam-input-helper"
    findInput   = title =? "Steam Linux input helper"
    manageInput = customFloating $ W.RationalRect l t w h
               where
                 h = 0.1
                 w = 0.2
                 t = 0.5 -h
                 l = 0.5 -w



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
     , isFullscreen -->  doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads




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
   , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
   , ("M-S-<Right>", shiftTo Next nonNSP >> moveTo Next nonNSP)        -- Shifts focused window to next ws
   , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws
   , ("M-S-<Left>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)         -- Shifts focused window to prev ws
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
   , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

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

   -- Scratchpads
    -- Toggle show/hide these programs.  They run on a hidden workspace.
    -- When you toggle them to show, it brings them to your current workspace.
    -- Toggle them to hide and it sends them back to hidden workspace (NSP).
   , ("<F12>", namedScratchpadAction myScratchPads "terminal")
   , ("S-<F12>", namedScratchpadAction myScratchPads "calculator")
   , ("M-<Backspace>", namedScratchpadAction myScratchPads "inputHelper")

   -- Controls
   -- TODO: Check that keys are correctly mapped
   , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
   , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@  -1.5%")
   , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
   , ("<XF86MonBrightnessUp>", spawn "light -A 5")
   , ("<XF86MonBrightnessDown>", spawn "light -U 5")
   ]
   -- The following lines are needed for named scratchpads.
   where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
         nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))



-- myXmobarPP :: PP
-- myXmobarPP = def
--      { ppSep              = magenta " • "
--      , ppTitleSanitize    = xmobarStrip
--      , ppCurrent          = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
--      , ppHidden           = white . wrap " " ""
--      , ppHiddenNoWindows  = lowWhite . wrap " " ""
--      , ppUrgent           = red . wrap (yellow "!") (yellow "!")
--      , ppOrder            = \[ws, l, _, wins] -> [ws, l, wins]
--      , ppExtras           = [logTitles formatFocused formatUnfocused]
--      }
--    where
--      formatFocused     = wrap (white      "[") (white      "]") . magenta . ppWindow
--      formatUnfocused   = wrap (lowWhite   "[") (lowWhite   "]") . blue    . ppWindow

--      -- | Windows should have *some* title, which should not not exceed a
--      -- sane length.
--      ppWindow :: String -> String
--      ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

--      blue, lowWhite, magenta, red, white, yellow :: String -> String
--      magenta  = xmobarColor "#ff79c6" ""
--      blue     = xmobarColor "#bd93f9" ""
--      white    = xmobarColor "#f8f8f2" ""
--      yellow   = xmobarColor "#f1fa8c" ""
--      red      = xmobarColor "#ff5555" ""
--      lowWhite = xmobarColor "#bbbbbb" ""




main :: IO ()
main = do
  -- Launching multiple instances of xmobar on their monitors. (if you have those many)
    xmproc  <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"

    -- the xmonad, ya know...what the WM is named after!
    xmonad $ docks . ewmh $ def
      { manageHook          = myManageHook <+> manageDocks
      , handleEventHook     = windowedFullscreenFixEventHook
      , modMask             = myModMask
      , terminal            = myTerminal
      , startupHook         = myStartupHook
      , layoutHook          = myLayout
      , workspaces          = myWorkspaces
      , borderWidth         = myBorderWidth
      , normalBorderColor    = myNormColor
      , focusedBorderColor  = myFocusColor
      , logHook = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
           -- The following variables begining with 'pp' are settings for xmobar
           { ppOutput       = \x -> hPutStrLn xmproc x                         -- xmobar on monitor 1
                                 >> hPutStrLn xmproc1 x                        -- xmobar on monitor 2

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
