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
      trayerCmd = "killall -9 trayer; trayer --edge bottom --align right --widthtype percent --transparent true --alpha 0 --tint 0x282c34 --width 10 --height 22 --iconspacing 6 --monitor primary &"
      ibusCmd = "ibus-daemon -drxR &"
      autorandr = "autorandr -c --default horizontal --skip-options crtc"
      --startSoundCmd = (mySoundPlayer ++ startupSound)

  sequence_ [ spawn wallpaperCmd
            , spawn trayerCmd
            , spawn autorandr
            --, spawn volumeIconCmd
            --, spawn shutterCmd
            --, spawnOnce ibusCmd
            --, setDefaultCursor xC_left_ptr
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
