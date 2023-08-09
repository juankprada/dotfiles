module Custom.MyDecorations where

import Colors.DoomOne
import XMonad (xK_Escape)
import XMonad qualified
import XMonad.Actions.EasyMotion
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Prompt

-- My Font
myFont :: String
myFont = "xft:Hack Nerd Font Mono:regular:size=10:antialias=true:hinting=true"

-- Sets border width for windows
myBorderWidth :: XMonad.Dimension
myBorderWidth = 2


myNormalBorderColor :: String
myNormalBorderColor = colorBack

myFocusedBorderColor :: String
myFocusedBorderColor = color15

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { bgColor = colorBack,
      fgColor = color01,
      bgHLight = color01,
      fgHLight = colorBack,
      historySize = 0,
      position = Top,
      borderColor = colorBack,
      promptBorderWidth = 0,
      defaultText = "",
      alwaysHighlight = True,
      height = 55,
      font = myFont,
      autoComplete = Nothing,
      showCompletionOnTab = False
    }

myShowWNameConfig :: SWNConfig
myShowWNameConfig =
  def
    { swn_font = myFont,
      swn_color = color01,
      swn_bgcolor = colorBack,
      swn_fade = 0.8

    }

myTabConfig :: Theme
myTabConfig =
  def
    {
      activeColor = color01,
      inactiveColor = colorBack,
      urgentColor = color02,
      activeBorderColor = colorBack,
      inactiveBorderColor = colorBack,
      urgentBorderColor = color02,
      activeTextColor = colorBack,
      inactiveTextColor = color03,
      urgentTextColor = colorBack,
      fontName = myFont
    }

emConf :: EasyMotionConfig
emConf =
  def
    { txtCol = color01,
      bgCol = color04,
      borderCol = color04,
      cancelKey = xK_Escape,
      emFont = myFont,
      overlayF = textSize,
      borderPx = 30
    }
