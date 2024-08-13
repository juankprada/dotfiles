module Custom.MyDefaults where

import XMonad

-- Sets modkey to Super/Windows key
myModMask :: KeyMask
myModMask = mod4Mask


-- Sets terminal to Alacritty
myTerminal :: String
myTerminal = "alacritty"

-- Sets my default browser
myBrowser :: String
myBrowser = "firefox"

-- Sets my default text editor
myEditor :: String
myEditor = "emacsclient -c -a 'emacs'"

-- The program that will play system sounds
mySoundPlayer :: String
mySoundPlayer = "ffplay -nodisp -autoexit"

soundDir = "/opt/system_sounds/" -- The directory that has the sound files

startupSound  = soundDir ++ "startup-01.mp3"
shutdownSound = soundDir ++ "shutdown-01.mp3"
dmenuSound    = soundDir ++ "menu-01.mp3"

-- Sets the border width for windows
myBorderWidth :: Dimension
myBorderWidth = 3
