module Custom.MyWorkspaces where

import Data.Maybe (fromJust)
import Data.Map as M

import qualified Data.Char as Char



-- TODO: I am a complete noob at Haskell. I need to find a way
-- to reduce duplication of workspace names
myWsIcons :: M.Map String String
myWsIcons = M.fromList [
    ("1:Web", "\xf059f"),
    ("2:Term", "\xf489"),
    ("3:Dev", "\xf121"),
    ("4:Music", "\xf075a"),
    ("5:Chat", "\xf0b7b"),
    ("6:Game", "\xf11b"),
    ("7:Art", "\xe22b"),
    ("8:Other", "\xf07c3")
  ]

myWorkspaces :: [String]
myWorkspaces = [
                 "1:Web"
               , "2:Term"
               , "3:Dev"
               , "4:Music"
               , "5:Chat"
               , "6:Game"
               , "7:Art"
               , "8:Other"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)


-- clickable :: [Char] -> [Char]
-- clickable ws = "<action=xdotool key super+"++show i++"><fn=1>"++ wsi ++"</fn>"++ws++"</action>"
--    where i = fromJust $ M.lookup ws myWorkspaceIndices
--          c = fromJust $ M.lookup wsi myWsIcons

clickable :: String -> String
clickable ws = "<action=xdotool key super+" ++ show i ++ "><fn=1>" ++ c ++ "</fn> " ++ ws ++ "</action> "
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices
    c = fromJust $ M.lookup ws myWsIcons



workspaceAt :: Int -> String
workspaceAt 0 = last myWorkspaces
workspaceAt n = ((myWorkspaces !!) . pred) n
