module Custom.MyManagement where

import Custom.MyDefaults (myTerminal, myBrowser, myEditor)
import Custom.MyScratchpads
import XMonad
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Util.NamedScratchpad

myManagement =
  composeAll
    [ (className =? "witcher3.exe" <&&> className =? "steam_app_0")   --> doCenterFloat,
      (className =? "xdg-desktop-portal-gnome")                       --> doCenterFloat,
      (className =? "confirm")                                        --> doFloat,
      (className =? "file_progress")                                  --> doFloat,
      (className =? "dialog")                                         --> doFloat,
      (className =? "download")                                       --> doFloat,
      (className =? "error")                                          --> doFloat,
      (className =? "notification")                                   --> doFloat,
      (className =? "splash")                                         --> doFloat,
      (className =? "toolbar")                                        --> doFloat,
      (className =? "re.sonny.Commit")                                --> doFloat,
      (appName =? "pavucontrol")                                      --> doFloat,
      (title   =? "MEGAsync" )                                        --> doFloat,
      (appName =? "keybase")                                          --> doFloat,
      (className  =? "Steam" <&&> fmap (/= "Steam") title)            --> doFloat,
      (title   =? "GameHub")                                          --> doFloat >> doShift "game",
      (appName =?  myBrowser)                                         --> doShift "www",
      (title   =? "Steam")                                            --> doShift "game",
      (className  =? "itch")                                          --> doShift "game",
      (className =? "firefox" <&&> resource =? "Dialog")              --> doCenterFloat -- Float Firefox Dialog,

    ]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <> myManagement
