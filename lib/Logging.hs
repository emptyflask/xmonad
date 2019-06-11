module Logging (eventLogHook, myWorkspaces) where

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

-- initWorkspaceLog = do
--   forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
--     safeSpawn "mkfifo" ["/tmp/" ++ file]

eventLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where
    fmt currWs ws
      | currWs == ws = "%{B#2c3e50}%{u#6e98a4} " ++ num ws ++ " %{B-}%{-u}"
      | otherwise    = " " ++ num ws ++ " "
    sort' = sortBy (compare `on` (!! 0))
    num = take 1

namedWorkspaces :: [(String, String)]
namedWorkspaces = [ ("Main",   "\xf120")
                  , ("Mail",   "\xf0e0")
                  , ("Chat",   "\xf0eb")
                  , ("Work-1", "\xf121")
                  , ("Work-2", "\xf121")
                  , ("Work-3", "\xf121")
                  , ("Work-4", "\xf121")
                  , ("Games",  "\xf11b")
                  , ("Music",  "\xf001")
                  ]

myWorkspaces = fst <$> namedWorkspaces
