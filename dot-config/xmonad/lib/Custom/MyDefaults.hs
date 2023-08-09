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
myEditor = "emacs"
