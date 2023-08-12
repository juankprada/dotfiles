module Custom.MyStartupApps where

import Custom.MyDefaults
import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Hooks.SetWMName


myStartupHook :: X ()
myStartupHook = do
  let
      wallpaperCmd = "nitrogen --restore"
      trayerCmd = "killall -9 trayer; trayer --edge top --align right --widthtype percent --transparent true --alpha 0 --tint 0x282c34 --width 10 --height 24 --iconspacing 5 --monitor primary"
      shutterCmd = "killall -9 shutter; shutter --min_at_startup"
      volumeIconCmd = "killall -9 volumeicon; volumeicon"
      nmAppletCmd = "killall -9 nm-applet; nm-applet"
      picomCmd = "killall -9 picom; picom -b"
      startSoundCmd = (mySoundPlayer ++ startupSound)

  sequence_ [spawnOnce startSoundCmd
            , spawn wallpaperCmd
            , spawn trayerCmd
            , spawn volumeIconCmd
            , spawn shutterCmd
            , setDefaultCursor xC_left_ptr
            ]






----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- myStartupHook = do                                                                                                                                                                 --
--   spawnOnce (mySoundPlayer ++ startupSound)                                                                                                                                        --
--   spawn "killall trayer"                                                                                                                                                           --
--   spawn "killall picom"                                                                                                                                                            --
--   spawn "autorandr -c --skip-options crtc"                                                                                                                                         --
--   spawnOnce "xsettingsd"                                                                                                                                                           --
--   spawnOnce "xsetroot -cursor_name left_ptr"                                                                                                                                       --
--   spawnOnce "[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap"                                                                                                                            --
--   spawnOnce "ibus-daemon -drxR"                                                                                                                                                    --
--   spawnOnce "unclutter"                                                                                                                                                            --
--   spawnOnce "lxsession"                                                                                                                                                            --
--   spawnOnce "volumeicon"                                                                                                                                                           --
--   spawnOnce "shutter --min_at_startup"                                                                                                                                             --
--   spawnOnce "emacs --daemon"                                                                                                                                                       --
--   spawnOnce "xscreensaver -no-splash"                                                                                                                                              --
--   spawnOnce "xfce4-power-manager"                                                                                                                                                  --
--   spawnOnce "dunst"                                                                                                                                                                --
--   spawnOnce "nm-applet"                                                                                                                                                            --
--   spawnOnce "nitrogen --restore"                                                                                                                                                   --
--   spawnOnce "sleep 2 && trayer --edge top --align right --widthtype percent --transparent true --alpha 0 --tint 0x282c34 --width 10 --height 24 --iconspacing 5 --monitor primary" --
--   spawnOnce "sleep 2 && picom -b"                                                                                                                                                  --
--   setWMName "LG3D"                                                                                                                                                                 --
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
