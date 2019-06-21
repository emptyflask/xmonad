module Logging (eventLogHook) where

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad
import XMonad
-- import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

-- import qualified Workspaces

-- initWorkspaceLog = do
--   forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
--     safeSpawn "mkfifo" ["/tmp/" ++ file]

eventLogHook = do
  winset <- gets windowset
  wintitle <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (wintitle ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where
    fmt currWs ws
      | currWs == ws = "%{B#2c3e50}%{u#6e98a4} " ++ num ws ++ " %{B-}%{-u}"
      | otherwise    = " " ++ num ws ++ " "
    sort' = sortBy (compare `on` (!! 0))
    num = take 1
