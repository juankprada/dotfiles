module Custom.MyManagement where


import XMonad
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Util.NamedScratchpad
import Custom.MyDefaults (myTerminal, myBrowser, myEditor)
import Custom.MyScratchpads
import Custom.MyWorkspaces

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
      (className =? "firefox")                                        --> doShift (workspaceAt 1),
      (title     =? "Steam")                                          --> doShift (workspaceAt 6),
      (className =? "Gimp")                                           --> doShift (workspaceAt 7),
      (title     =? "Krita")                                          --> doShift (workspaceAt 7),
      (className =? "firefox" <&&> resource =? "Dialog")              --> doFloat,  -- Float Firefox Dialog
      (isFullscreen)                                                  --> doFullFloat
    ]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <> myManagement
