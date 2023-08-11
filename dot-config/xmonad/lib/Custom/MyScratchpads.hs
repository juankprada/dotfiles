module Custom.MyScratchpads where

import Custom.MyDefaults
import Custom.MyManagementPositioning

import XMonad
import XMonad.ManageHook ((=?))
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [
    NS "terminal" spawnTerm findTerm manageTerm
  , NS "mocp" spawnMocp findMocp manageMocp
  , NS "calculator" spawnCalc findCalc manageCalc
    --NS "quick commands" spawnQc findQc myCenter
  ]
  where
    spawnTerm = myTerminal ++ " -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                   h = 0.9
                   w = 0.9
                   t = 0.95 -h
                   l = 0.95 -w
    spawnMocp  = myTerminal ++ " -t mocp -e mocp"
    findMocp   = title =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w

{-
To get WM_CLASS of a visible window, run "xprop | grep 'CLASS'" and select the window.
appName :: Query String
Return the application name; i.e., the first string returned by WM_CLASS.

resource :: Query String
Backwards compatible alias for appName.

className :: Query String
Return the resource class; i.e., the second string returned by WM_CLASS. -}
