{-# LANGUAGE ImportQualifiedPost #-}

module Custom.MyKeys where

import Custom.MyDefaults
import Custom.MyDecorations
import Custom.MyScratchpads
import Custom.MyWorkspaces

import Data.Map qualified as M
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.EasyMotion
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Search qualified as S
import XMonad.Actions.Submap qualified as SM
import XMonad.Actions.WithAll
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.BoringWindows
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation as WN
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Man
import XMonad.Prompt.XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S


import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation


myKeys :: [(String, X ())]
myKeys =
  -- XMonads
   [ -- Terminal
     ("M-<Return>", spawn myTerminal),
     -- Browser
     ("M-b", spawn myBrowser),

     -- Editor
     ("M-e", spawn myEditor),

     -- Rofi
     ("M-p", spawn "rofi -show drun"),
     ("M-y", spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"),
     ("M1-<Tab>", spawn "rofi -show window"),


     -- Shutter
     ("<Print>", spawn "shutter"),

     -- Prompt
     ("M-x", xmonadPrompt myXPConfig,
     -- Search commands (wait for next keypress)
     ("M-s", SM.submap $ searchEngineMap $ S.promptSearchBrowser myPromptConfig myBrowser),
     
     -- NamedScratchpads TODO: Fix this
     ("M-t", namedScratchpadAction myScratchpads "terminal"),
        -- Kill windows
     ("M-S-c", kill1),     -- Kill the currently focused client
     ("M-S-a", killAll),   -- Kill all windows on current workspace
     -- Layouts
     ("M-<Tab>", sendMessage NextLayout),           -- Switch to next layout
     ("M-<Space>", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles noborder/full

     -- cycle workspaces
     ("M-<Right>", nextWS),
     ("M-<Left>", prevWS),
     ("M-S-<Right>", shiftToNext),
     ("M-S-<Left>", shiftToPrev),
     ("M-.", nextScreen),  -- Switch focus to next monitor
     ("M-,", prevScreen),  -- Switch focus to prev monitor
     ("M-S-.", shiftNextScreen),
     ("M-S-,", shiftPrevScreen),
     ("M-S-1", windows $ W.shift $ myWorkspaces !! 0),                  -- Send to workspace 1
     ("M-S-2", windows $ W.shift $ myWorkspaces !! 1),                  -- Send to workspace 2
     ("M-S-3", windows $ W.shift $ myWorkspaces !! 2),                  -- Send to workspace 3
     ("M-S-4", windows $ W.shift $ myWorkspaces !! 3),                  -- Send to workspace 4
     ("M-S-5", windows $ W.shift $ myWorkspaces !! 4),                  -- Send to workspace 5
     ("M-S-6", windows $ W.shift $ myWorkspaces !! 5),                  -- Send to workspace 6
     ("M-S-7", windows $ W.shift $ myWorkspaces !! 6),                  -- Send to workspace 7
     ("M-S-8", windows $ W.shift $ myWorkspaces !! 7),                  -- Send to workspace 8
     ("M-S-9", windows $ W.shift $ myWorkspaces !! 8),                  -- Send to workspace 9
     ("M-S-z", toggleWS),
      -- Windows navigation
     ("M-m", windows W.focusMaster),  -- Move focus to the master window
     ("M-j", windows W.focusDown),    -- Move focus to the next window
     ("M-k", windows W.focusUp),      -- Move focus to the prev window
     ("M-S-m", windows W.swapMaster), -- Swap the focused window and the master window
     ("M-S-j", windows W.swapDown),   -- Swap focused window with next window
     ("M-S-k", windows W.swapUp),     -- Swap focused window with prev window
     ("M-S-<Tab>", rotSlavesDown),    -- Rotate all windows except master and keep focus in place
     ("M-C-<Tab>", rotAllDown),       -- Rotate all the windows in the current stack


     -- Increase/decrease windows in the master pane or the stack
     ("M-S-<Up>", sendMessage (IncMasterN 1)),      -- Increase # of clients master pane
     ("M-S-<Down>", sendMessage (IncMasterN (-1))), -- Decrease # of clients master pane
     ("M-C-<Up>", increaseLimit),                   -- Increase # of windows
     ("M-C-<Down>", decreaseLimit),                 -- Decrease # of windows

     -- Window resizing
     ("M-h", sendMessage Shrink),                   -- Shrink horiz window width
     ("M-l", sendMessage Expand),                   -- Expand horiz window width
     ("M-M1-j", sendMessage MirrorShrink),          -- Shrink vert window width
     ("M-M1-k", sendMessage MirrorExpand),          -- Expand vert window width


     -- Xmonad
     ("M-S-r", spawn "xmonad --recompile && xmonad --restart"),    -- Restarts xmonad
     --("M-S-q", io exitSuccess),              -- Quits xmonad
     ("M-S-q", confirmPrompt def "exit" (io (exitWith ExitSuccess)) ),

        -- Controls
     ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%"),
     ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@  -1.5%"),
     ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
     ("<XF86MonBrightnessUp>", spawn "light -A 5"),
     ("<XF86MonBrightnessDown>", spawn "light -U 5"),

     -- Floating windows
     ("M-f", sendMessage (T.Toggle "floats")), -- Toggles my 'floats' layout
     ("M-t", withFocused $ windows . W.sink),  -- Push floating window back to tile
     ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

   ]


searchEngineMap method =
  M.fromList
    [ ((0, xK_a), method $ S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search="),
      ((0, xK_d), method S.duckduckgo),
      ((0, xK_g), method S.google),
      ((0, xK_h), method S.hoogle),
      ((0, xK_i), method S.imdb),
      ((0, xK_s), method $ S.searchEngine "stackoverflow" "https://stackoverflow.com/search?q="),
      ((0, xK_w), method S.wikipedia),
      ((0, xK_y), method S.youtube)
    ]


myXPConfig :: XPConfig
myXPConfig =
  def
    { font =
        "xft:Source Code Pro:size=11:regular:antialias=true,FontAwesome:pixelsize=13"
    , position = CenteredAt (1 / 4) (2 / 3)
    , bgColor = myBgColor
    , fgColor = myFgColor
    , bgHLight = myFgColor
    , fgHLight = myBgColor
    , borderColor = myFgColor
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    , height = 30
    }
