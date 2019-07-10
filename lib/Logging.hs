module Logging (eventLogHook) where

import Data.List (sort)
-- import Data.Function (on)
import Control.Monad
import XMonad
-- import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

import qualified Workspaces

-- initWorkspaceLog = do
--   forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
--     safeSpawn "mkfifo" ["/tmp/" ++ file]

eventLogHook :: X ()
eventLogHook = do
  winset <- gets windowset
  wintitle <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map ((Workspaces.polybarString $ read currWs) . read) $ sort wss 

  io $ appendFile "/tmp/.xmonad-title-log" (wintitle ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  -- where
  --   sort' = sortBy (compare `on` (!! 0))
--     fmt currWs ws
--       | currWs == ws = "%{B#2c3e50}%{u#6e98a4} " ++ num ws ++ " %{B-}%{-u}"
--       | otherwise    = " " ++ num ws ++ " "
--     num = take 1
