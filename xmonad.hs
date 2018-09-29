-------------------------------------------------------------------------------
-- Configuration for using xmonad inside xfce.
--
-- 1. Start xmonad by adding it to "Application Autostart" in xfce.
-- 2. Make sure xfwm4 is disabled from autostart, or uninstalled.
-- 3. Make sure xfdesktop is disabled from autostart, or uninstalled
--    since it may prevent xfce-panel from appearing once xmonad is started.
-------------------------------------------------------------------------------

import XMonad
import XMonad.Config.Xfce (desktopLayoutModifiers, xfceConfig)

import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Actions.ShowText (handleTimerEvent)

import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.ShowWName

-- import System.Taffybar.Hooks.PagerHints (pagerHints)

import Keys (myKeys)
import Layout (myLayoutHook)
import Managers (pbManageHook, myManageHook)

main :: IO ()
main = 
  let
      conf = xfceConfig
        { borderWidth        = 2
        , clickJustFocuses   = False
        , clientMask         = clientMask xfceConfig
        , focusFollowsMouse  = True
        , focusedBorderColor = "#990000"

        , handleEventHook    = handleEventHook xfceConfig
                               <+> fullscreenEventHook
                               <+> handleTimerEvent

        , handleExtraArgs    = handleExtraArgs xfceConfig
        , keys               = myKeys
        , layoutHook         = desktopLayoutModifiers $ showWName myLayoutHook
        , logHook            = logHook xfceConfig
                               <+> historyHook

        , manageHook         = manageHook xfceConfig
                               <+> myManageHook
                               <+> manageDocks
                               <+> pbManageHook

        , modMask            = mod4Mask
        , mouseBindings      = mouseBindings xfceConfig
        , normalBorderColor  = "#444444"
        , rootMask           = rootMask xfceConfig

        , startupHook        = startupHook xfceConfig
                               >> setWMName "LG3D" -- Java app focus fix

        , terminal           = "qterminal"

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
   in
    xmonad $ ewmh conf
