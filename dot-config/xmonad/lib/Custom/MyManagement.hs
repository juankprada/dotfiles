module Custom.MyManagement where

import Custom.MyDefaults (myTerminal, myBrowser, myEditor)
import Custom.MyScratchpads
import XMonad
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Util.NamedScratchpad

myManagement =
  composeAll
    [
      (className =? "confirm")                                        --> doFloat,
      (className =? "file_progress")                                  --> doFloat,
      (className =? "dialog")                                         --> doFloat,
      (className =? "download")                                       --> doFloat,
      (className =? "error")                                          --> doFloat,
      (className =? "notification")                                   --> doFloat,
      (className =? "pinentry-gtk-2")                                 --> doFloat,
      (className =? "Yad")                                            --> doFloat,
      (className =? "splash")                                         --> doFloat,
      (className =? "toolbar")                                        --> doFloat,
      (className =? "re.sonny.Commit")                                --> doFloat,
      (title     =? "Mozilla Firefox")                                --> doShift " www ",
      (title     =? "Steam")                                          --> doShift " game ",
      (className =? "Gimp")                                           --> doShift " art ",
      (className =? "Krita")                                          --> doShift " art ",
      (className =? "firefox" <&&> resource =? "Dialog")              --> doFloat,  -- Float Firefox Dialog
      (isFullscreen)                                                  --> doFullFloat
    ]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <> myManagement
