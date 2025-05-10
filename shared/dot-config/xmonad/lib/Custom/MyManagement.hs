module Custom.MyManagement where


import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.FloatNext
import XMonad.Layout.NoBorders
import XMonad.Util.NamedScratchpad
import Custom.MyDefaults (myTerminal, myBrowser, myEditor)
import Custom.MyScratchpads
import Custom.MyWorkspaces

myManagement =
  composeAll
    [ isFullscreen --> doFullFloat
    , placeHook (fixed (0.5, 0.5))
    , floatNextHook
    , appName =? "blueman-manager" --> doFloat
    , appName =? "pavucontrol" --> doFloat
    , appName =? "zenity" --> doFloat
    , appName =? "qimgv" --> doFloat
    , appName =? "peek" --> doFloat <> hasBorder False
    , className =? "displaycal" -->doFloat
    , className =? "Xarchiver" --> doFloat
    , className =? "firefox" --> doShift (workspaceAt 1)
    , className =? "steam" --> doShift (workspaceAt 6)
    , title =? "Krita" --> doShift (workspaceAt 7)
    , className =? "Gimp" --> doShift (workspaceAt 7)
    , className =? "Lazarus" --> doFloat
    , isDialog --> doFloat
    , title =? "Pathseekers" --> doFloat
    , className =? "Pathseekers" --> doFloat
    , appName =? "Pathseekers" --> doFloat
    , transience'
    
      -- (className =? "confirm")                                        --> doFloat,
      -- (className =? "file_progress")                                  --> doFloat,
      -- (className =? "dialog")                                         --> doFloat,
      -- (className =? "download")                                       --> doFloat,
      -- (className =? "error")                                          --> doFloat,
      -- (className =? "notification")                                   --> doFloat,
      -- (className =? "pinentry-gtk-2")                                 --> doFloat,
      -- (className =? "Yad")                                            --> doFloat,
      -- (className =? "splash")                                         --> doFloat,
      -- (className =? "toolbar")                                        --> doFloat,
      -- (className =? "re.sonny.Commit")                                --> doFloat,
      -- (className =? "firefox")                                        --> doShift (workspaceAt 1),
      -- (title     =? "Steam")                                          --> doShift (workspaceAt 6),
      -- (className =? "Gimp")                                           --> doShift (workspaceAt 7),
      -- (title     =? "Krita")                                          --> doShift (workspaceAt 7) . doFullFloat,
      -- (className =? "displaycal")                                     --> doFloat,
      -- (className =? "thunar" <&&> title=? "File Operation Progress")  --> doFloat,
      -- (className =? "firefox" <&&> resource =? "Dialog")              --> doFloat,  -- Float Firefox Dialog
      -- (className =? "qimgv")                                          --> doFloat,
      -- (className =? "Xarchiver")                                      --> doFloat,
      -- (className =? "Galculator")                                     --> doFloat,
      -- (resource =? "trayer")                                          --> doIgnore,
      -- (isFullscreen)                                                  --> doFullFloat
    ]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <> myManagement
