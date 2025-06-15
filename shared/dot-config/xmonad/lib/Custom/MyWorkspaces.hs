module Custom.MyWorkspaces where

import Data.Maybe (fromJust)
import Data.Map as M

import qualified Data.Char as Char



-- TODO: I am a complete noob at Haskell. I need to find a way
-- to reduce duplication of workspace names
myWsIcons :: M.Map String String
myWsIcons = M.fromList [
    ("Web", "\xf269"),
    ("Term", "\xf489"),
    ("Dev", "\xe632"),
    ("File", "\xf07c"),
    ("Chat", "\xf0b79"),
    ("Game", "\xf11b"),
    ("Art", "\xf03d8"),
    ("Other", "\xf292")
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
clickable ws = " <action=xdotool key super+" ++ show i ++ ">"++ c ++"</action> "
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices
    c = fromJust $ M.lookup ws myWsIcons

workspaceAt :: Int -> String
workspaceAt 0 = last myWorkspaces
workspaceAt n = ((myWorkspaces !!) . pred) n
