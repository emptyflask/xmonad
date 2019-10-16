import XMonad
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)

import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Actions.Navigation2D
import XMonad.Actions.ShowText (handleTimerEvent)

import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(..))
-- import XMonad.Hooks.SetWMName (setWMName)

import XMonad.Util.Run (safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnOnce)

-- import System.Taffybar.Hooks.PagerHints (pagerHints)

import Keys (myKeys)
import Layout (myLayoutHook)
import Managers (pbManageHook, myManageHook)
import Logging (eventLogHook)
import qualified Workspaces

main :: IO ()
main =
  do
    safeSpawn "mkfifo" ["/tmp/.xmonad-workspace-log"]
    safeSpawn "mkfifo" ["/tmp/.xmonad-title-log"]

    xmonad
      $ withUrgencyHook NoUrgencyHook
      $ ewmh 
      $ withNavigation2DConfig def
        { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
        }
      $ desktopConfig
        { borderWidth        = 2
        , clickJustFocuses   = False
        , clientMask         = clientMask desktopConfig
        , focusFollowsMouse  = False
        , focusedBorderColor = "#990000"

        , handleEventHook    = handleEventHook desktopConfig
                                <+> fullscreenEventHook
                                <+> handleTimerEvent

        , handleExtraArgs    = handleExtraArgs desktopConfig
        , keys               = myKeys
        , layoutHook         = desktopLayoutModifiers myLayoutHook
        , logHook            = logHook desktopConfig
                                <+> historyHook
                                <+> eventLogHook

        , manageHook         = manageHook desktopConfig
                                <+> myManageHook
                                <+> pbManageHook

        , modMask            = mod4Mask
        , mouseBindings      = mouseBindings desktopConfig
        , normalBorderColor  = "#444444"
        , rootMask           = rootMask desktopConfig

        , startupHook        = do
                                 startupHook desktopConfig
                                 spawnOnOnce "2" "thunderbird"
                                 spawnOnOnce "3" "slack"
                                 spawnOnOnce "3" "signal-desktop"
                                 spawnOnOnce "9" "spotify"
                                -- >> setWMName "LG3D" -- Java app focus fix

        , terminal           = "kitty"

        , workspaces         = Workspaces.numbered
        }
