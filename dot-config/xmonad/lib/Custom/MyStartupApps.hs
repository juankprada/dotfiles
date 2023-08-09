module Custom.MyStartupApps where

import XMonad
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  let wallpaperCmd = "nitrogern --restore &"
      picomCmd = "killall -9 picom; sleep 2 && picom -b &"
      trayerCmd = "killall -9 trayer; trayer --edge top --align right --widthtype percent --transparent true --alpha 0 --tint 0x282c34 --width 10 --height 24 --iconspacing 5 --monitor primary &"
      shutterCmd = "killall -9 shutter; shutter -min_at_startup &"
      volumeIconCmd = "killall -9 volumeicon; volumeicon &"
      nmAppletCmd = "killall -9 nm-applet; nm-applet &"
  sequence_ [spawn wallpaperCmd, spawn picomCmd, spawn trayerCmd, spawn shutterCmd, spawn volumeIconCmd, spawn nmAppletCmd]
