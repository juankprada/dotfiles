module Custom.MyScreen where

import XMonad
import XMonad.Hooks.Rescreen


myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "sleep 1; xmonad --restart"

myRandrChangeHook :: X ()
--myRandrChangeHook = spawn "sleep 1;" --"; autorandr -c --default horizontal --skip-options crtc"
myRandrChangeHook = spawn "sleep 1; autorandr -c --default horizontal --skip-options crtc"

rescreenCfg :: RescreenConfig
rescreenCfg =
  def
    { afterRescreenHook = myAfterRescreenHook,
      randrChangeHook = myRandrChangeHook
    }
    
