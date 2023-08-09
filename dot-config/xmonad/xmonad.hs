import XMonad
import System.Directory
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions

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
import XMonad.Hooks.Rescreen (rescreenHook)

    -- Layouts


import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances


    -- Layouts modifiers




   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook, trayPaddingEventHook)
import XMonad.Util.NamedActions
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

import XMonad.Prompt.ConfirmPrompt

import Custom.MyDefaults (myModMask, myTerminal, myBrowser, myEditor)
import Custom.MyDecorations (myBorderWidth, myFocusedBorderColor, myNormalBorderColor)
import Custom.MyScreen (rescreenCfg)
import Custom.MyStartupApps (myStartupHook)
import Custom.MyWorkspaces (myWorkspaces, clickable)
import Custom.MyLayouts (myLayoutHook)
import Custom.MyManagement
import Custom.MyKeys (myKeys)

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset



myEventHook = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty"  <||> className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook


main :: IO ()
main = do
  -- Launching multiple instances of xmobar on their monitors. (if you have those many)
    xmproc  <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
  --    xmproc_bt1 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc-bot"
    xmonad
      $ javaHack
        . rescreenHook rescreenCfg
      $ docks
        . ewmh
      $ def
        {
          terminal = myTerminal
        , focusFollowsMouse  = False -- Dont change focus when mose moves to other window
        , borderWidth         = myBorderWidth
        , modMask             = myModMask -- Sets the Mod Key to "Super"
        , workspaces          = myWorkspaces
        , normalBorderColor    = myNormalBorderColor
        , focusedBorderColor  = myFocusedBorderColor
        , startupHook = myStartupHook
        , manageHook          = myManageHook <+> manageDocks
        , layoutHook          = myLayoutHook
        , handleEventHook     = myEventHook
        --- NOT MIGRATED

        , logHook = dynamicLogWithPP $ xmobarPP
                    -- The following variables begining with 'pp' are settings for xmobar
                    { ppOutput       = \x -> hPutStrLn xmproc x                         -- xmobar on monitor 1
                                            -- >> hPutStrLn xmproc_bt1 x                        -- xmobar bottom on monitor 1
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
