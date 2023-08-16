module Custom.MyWorkspaces where

import Data.Maybe (fromJust)
import Data.Map as M

myWorkspaces :: [String]
myWorkspaces = ["1.www", "2.terminal", "3.dev", "4.music", "5.chat", "6.game", "7.art", "8.other"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
   where i = fromJust $ M.lookup ws myWorkspaceIndices
