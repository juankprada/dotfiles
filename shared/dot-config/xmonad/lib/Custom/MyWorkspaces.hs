module Custom.MyWorkspaces where

import Data.Maybe (fromJust)
import Data.Map as M

import qualified Data.Char as Char



-- TODO: I am a complete noob at Haskell. I need to find a way
-- to reduce duplication of workspace names
myWsIcons :: M.Map String String
myWsIcons = M.fromList [
    ("Web", "\xf059f"),
    ("Term", "\xf489"),
    ("Dev", "\xf121"),
    ("File", "\xe5fe"),
    ("Chat", "\xf0b7b"),
    ("Game", "\xf11b"),
    ("Art", "\xe22b"),
    ("Other", "\xf07c3")
  ]

myWorkspaces :: [String]
myWorkspaces = [
                 "Web"
               , "Term"
               , "Dev"
               , "File"
               , "Chat"
               , "Game"
               , "Art"
               , "Other"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] 

clickable :: String -> String
clickable ws = "<action=xdotool key super+" ++ show i ++ ">"++ show i ++ ":<fn=1>" ++ c ++ "</fn></action> "
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices
    c = fromJust $ M.lookup ws myWsIcons

workspaceAt :: Int -> String
workspaceAt 0 = last myWorkspaces
workspaceAt n = ((myWorkspaces !!) . pred) n
