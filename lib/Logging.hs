module Logging (eventLogHook) where

import Text.Read (readMaybe)
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
  winset   <- gets windowset
  wintitle <- maybe (return "") (fmap show . getName) . W.peek $ winset

  let currWs = maybe 1 id (readMaybe . W.currentTag $ winset)
  let allWs = [1 .. (length Workspaces.workspaces)]

  let wsString = join $ map (Workspaces.polybarString currWs) allWs

  io $ appendFile "/tmp/.xmonad-title-log" (wintitle ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsString ++ "\n")
