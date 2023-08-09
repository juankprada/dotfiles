module Custom.MyScreen where

import XMonad
import XMonad.Hooks.Rescreen


myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "sleep1; xmonad --restart"

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change -d common --skip-options crtc"

rescreenCfg :: RescreenConfig
rescreenCfg =
  def
    { afterRescreenHook = myAfterRescreenHook,
      randrChangeHook = myRandrChangeHook
    }
