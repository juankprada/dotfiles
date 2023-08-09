module Custom.MyWorkspaces where

import Data.Maybe (fromJust)
import Data.Map as M

myWorkspaces :: [String]
myWorkspaces = ["www", "terminal", "dev", "music", "slack", "game", "art", "other"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices
