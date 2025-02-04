module Custom.MyDecorations where

import Colors.DoomOne
import XMonad (xK_Escape)
import XMonad
import XMonad.Actions.EasyMotion
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Prompt

-- My Font
myFont :: String
myFont = "xft:Terminess Nerd Font:regular:size=12:antialias=true:hinting=true"

-- Sets border width for windows
myBorderWidth :: XMonad.Dimension
myBorderWidth = 4

myNormalBorderColor :: String
myNormalBorderColor = colorBack

myFocusedBorderColor :: String
myFocusedBorderColor = color15

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { bgColor = colorBack,
      fgColor = color03,
      bgHLight = color02,
      fgHLight = colorBack,
      historySize = 0,
      position = Top,
      borderColor = color03,
      promptBorderWidth = 2,
      defaultText = "",
      alwaysHighlight = True,
      height = 45,
      font = myFont,
      autoComplete = Nothing,
      showCompletionOnTab = False
    }

myShowWNameConfig :: SWNConfig
myShowWNameConfig =
  def
    { swn_font = "xft:Terminess Nerd Font:regular:size=10:antialias=true:hinting=true",
      swn_color = color04,
      swn_bgcolor = colorBack,
      swn_fade = 0.8
    }

myTabConfig :: Theme
myTabConfig =
  def
    {
      activeColor = colorBack,
      inactiveColor = color09,
      urgentColor = color02,
      activeBorderColor = colorBack,
      inactiveBorderColor = colorBack,
      urgentBorderColor = color02,
      activeTextColor = color03,
      inactiveTextColor = colorFore,
      urgentTextColor = colorBack,
      fontName = "xft:Terminess Nerd Font:regular:size=10:antialias=true:hinting=true"
    }

-- emConf :: EasyMotionConfig
-- emConf =
--   def
--     { txtCol = color02,
--       bgCol = color04,
--       borderCol = color04,
--       cancelKey = xK_Escape,
--       emFont = myFont,
--       overlayF = textSize,
--       borderPx = 30
--     }
