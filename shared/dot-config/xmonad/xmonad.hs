import XMonad
import System.Directory
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace hiding (
  TI,
  TopicItem,
  topicNames,
 )
import Foreign.C (CInt)

    -- Actions

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.Rescreen (rescreenHook)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Modal
import XMonad.Hooks.Rescreen
import XMonad.Hooks.FadeInactive

import XMonad.Prelude

 -- Layouts
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens

    -- Layouts modifiers
import XMonad.Actions.UpdatePointer

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Hacks
import XMonad.Util.NamedActions
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.NamedScratchpad
import XMonad.Prompt.ConfirmPrompt

import Custom.MyDefaults (myModMask, myTerminal, myBrowser, myEditor)
import Custom.MyDecorations (myBorderWidth, myFocusedBorderColor, myNormalBorderColor)
import Custom.MyScreen (rescreenCfg)
import Custom.MyStartupApps (myStartupHook)
import Custom.MyWorkspaces
import Custom.MyLayouts (myLayoutHook, myLayoutHook2)
import Custom.MyManagement
import Custom.MyKeys (myKeys)
import Colors.Nord

import Data.List

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


myEventHook :: Event -> X All
myEventHook = multiScreenFocusHook
  <> windowedFullscreenFixEventHook
  <> swallowEventHook (className =? "Alacritty"  <||> className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook

multiScreenFocusHook :: Event -> X All
multiScreenFocusHook MotionEvent { ev_x = x, ev_y = y } = do
  ms <- getScreenForPos x y
  case ms of
    Just cursorScreen -> do
      let cursorScreenID = W.screen cursorScreen
      focussedScreenID <- gets (W.screen . W.current . windowset)
      when (cursorScreenID /= focussedScreenID) (focusWS $ W.tag $ W.workspace cursorScreen)
      return (All True)
    _ -> return (All True)
  where getScreenForPos :: CInt -> CInt
            -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
        getScreenForPos x y = do
          ws <- windowset <$> get
          let screens = W.current ws : W.visible ws
              inRects = map (inRect x y . screenRect . W.screenDetail) screens
          return $ fst <$> find snd (zip screens inRects)
        inRect :: CInt -> CInt -> Rectangle -> Bool
        inRect x y rect = let l = fromIntegral (rect_x rect)
                              r = l + fromIntegral (rect_width rect)
                              t = fromIntegral (rect_y rect)
                              b = t + fromIntegral (rect_height rect)
                           in x >= l && x < r && y >= t && y < b
        focusWS :: WorkspaceId -> X ()
        focusWS ids = windows (W.view ids)
multiScreenFocusHook _ = return (All True)

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9


main :: IO ()
main = do
    xmonad
      . ewmhFullscreen
      . ewmh
      . dynamicSBs barSpawner
      . javaHack
      . rescreenHook rescreenCfg
      . docks
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
        , manageHook          = myManageHook -- <+> manageDocks
        , layoutHook          = myLayoutHook
        , handleEventHook     = myEventHook
        , logHook             = myLogHook
        } `additionalKeysP` myKeys


----------------------------------------
-- Status Bars
----------------------------------------

barSpawner :: Applicative f => ScreenId -> f StatusBarConfig
barSpawner (S s) = do
    if s == 0
      then pure $ topBarConfig <> bottomBarConfig -- On Main Screen we hae two bars
      else pure $ topBarConfig -- On other screens we only have one bar
    where
      topBarConfig = statusBarPropTo "_XMONAD_LOG_1" ("xmobar -x " ++ show s ++ " ~/.config/xmobar/xmobarrc") (pure $ myXmobarPP (S s))
      bottomBarConfig = statusBarPropTo "_XMONAD_LOG_2" ("xmobar -x " ++ show s ++ " ~/.config/xmobar/xmobar_bottom") (pure $ myXmobarBottomPP (S s))

-- barSpawner :: ScreenId -> IO StatusBarConfig
-- barSpawner 0 = pure $ xmobarTop <> xmobarBottom -- two bars in the main screen
-- barSpawner 1 = pure $ xmobarTop <> xmobarBottom -- two bars in second screen
-- barSpanwer _ = pure $ xmobarBottom


-- xmobarTop    = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.config/xmobar/xmobarrc"       (pure $ myXmobarPP 0)
-- xmobarBottom = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 0 ~/.config/xmobar/xmobar_bottom"  (pure $ myXmobarBottomPP 0)


activeWsColor :: String -> String
activeWsColor = xmobarColor color07 ""

windowTitleColor :: String -> String
windowTitleColor = xmobarColor color04 ""


urgentColor :: String -> String
urgentColor = xmobarColor color02 ""

blue :: String -> String
blue     = xmobarColor "#bd93f9" ""

lowWhite :: String -> String
lowWhite = xmobarColor "#bbbbbb" ""

red :: String -> String
red      = xmobarColor "#ff5555" ""

white :: String -> String
white    = xmobarColor "#f8f8f2" ""

yellow :: String -> String
yellow   = xmobarColor "#f1fa8c" ""

myXmobarPP :: ScreenId -> PP
myXmobarPP s = filterOutWsPP [scratchpadWorkspaceTag] $ def
  {
    ppCurrent      = activeWS
  , ppSep =  "<fc=#666666> | </fc>"                                     -- Separator character
  , ppVisible      = visibleWS
  , ppHidden       = hiddenWS
  , ppHiddenNoWindows = hiddenNoWWS
  , ppUrgent = urgentWS
  , ppTitleSanitize = xmobarStrip
  , ppExtras =  [windowCount]
  , ppOrder  = \(ws:l:t:ex) -> [ws]
 }
 where
   activeWS = xmobarBorder "Bottom" color07 3 . activeWsColor . clickable           -- current workspace
   visibleWS = xmobarColor color04 "" . clickable -- Visible but not current workspace
   hiddenWS = xmobarBorder "Bottom" color06 1 . xmobarColor color06 "" . clickable --Hidden workspaces
   hiddenNoWWS = xmobarColor colorFore ""  . clickable           -- Hidden workspaces (no windows)
   urgentWS = xmobarColor color02 "" . wrap "!" "!"  . clickable                 -- Urgent workspace
   formatFocused   = xmobarBorder "Bottom" "#ff79c6" 2 . activeWsColor . ppWindow
   formatUnfocused = wrap (lowWhite "") (lowWhite "") . blue    . ppWindow
   ppWindow :: String -> String
   ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

myXmobarBottomPP :: ScreenId -> PP
myXmobarBottomPP s = filterOutWsPP [scratchpadWorkspaceTag] $ def
  {
  ppSep =  "<fc=#666666> | </fc>"                                     -- Separator character
  , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
  , ppTitleSanitize = xmobarStrip
  , ppExtras = [windowCount, logTitles' formatedWindows ]  -- # of windows current workspace
  , ppOrder = \(ws:l:t:ex) -> [l]++ex
  }
 where
   formatUrgent    = wrap (white    "") (lowWhite    " |") . xmobarBorder "Bottom" color02 2 . urgentColor . ppWindow
   formatFocused   = wrap (white    "") (lowWhite    " |") . xmobarBorder "Bottom" "#ff79c6" 2 . activeWsColor . ppWindow
   formatUnfocused = wrap (lowWhite "") (lowWhite " |") . blue . ppWindow
   formatedWindows = TitlesFormat { focusedFormat = formatFocused, unfocusedFormat = formatUnfocused, urgentFormat = formatUrgent }
   ppWindow :: String -> String
   ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 60




