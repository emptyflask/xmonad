import XMonad
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)

import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Actions.Navigation2D
import XMonad.Actions.ShowText (handleTimerEvent)

import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(..))
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.ShowWName

import XMonad.Util.Run (safeSpawn)

-- import System.Taffybar.Hooks.PagerHints (pagerHints)

import Keys (myKeys)
import Layout (myLayoutHook)
import Managers (pbManageHook, myManageHook)
import Logging (eventLogHook, myWorkspaces)

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
        , layoutHook         = desktopLayoutModifiers $ showWName myLayoutHook
        , logHook            = logHook desktopConfig
                                <+> historyHook
                                <+> eventLogHook

        , manageHook         = manageHook desktopConfig
                                <+> myManageHook
                                <+> manageDocks
                                <+> pbManageHook

        , modMask            = mod4Mask
        , mouseBindings      = mouseBindings desktopConfig
        , normalBorderColor  = "#444444"
        , rootMask           = rootMask desktopConfig

        , startupHook        = startupHook desktopConfig
                                -- >> setWMName "LG3D" -- Java app focus fix

        , terminal           = "kitty"

        , workspaces         = [ "1: Main"
                              , "2: Mail"
                              , "3: Chat"
                              , "4: Work-1"
                              , "5: Work-2"
                              , "6: Work-3"
                              , "7: Work-4"
                              , "8: Games"
                              , "9: Music"
                              ]
        }
