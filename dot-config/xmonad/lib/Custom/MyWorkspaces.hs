module Custom.MyWorkspaces where

import Data.Maybe (fromJust)
import Data.Map as M

myWorkspaces :: [String]
myWorkspaces = ["<fn=1>\xf059f</fn> www", "<fn=1>\xf489</fn> term", "<fn=1>\xf121</fn> dev", "<fn=1>\xf075a</fn> music", "<fn=1>\xf0b7b</fn> chat", "<fn=1>\xf11b</fn> game", "<fn=1>\xe22b</fn> art", "<fn=1>\xf07c3</fn> other"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
   where i = fromJust $ M.lookup ws myWorkspaceIndices

workspaceAt :: Int -> String
workspaceAt 0 = last myWorkspaces
workspaceAt n = ((myWorkspaces !!) . pred) n
