-------------------------------------------------------------------------------
-- Configuration for using xmonad inside xfce.
--
-- 1. Start xmonad by adding it to "Application Autostart" in xfce.
-- 2. Make sure xfwm4 is disabled from autostart, or uninstalled.
-- 3. Make sure xfdesktop is disabled from autostart, or uninstalled
--    since it may prevent xfce-panel from appearing once xmonad is started.
-------------------------------------------------------------------------------

import XMonad
import XMonad.Config.Xfce

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.SetWMName (setWMName)

import Keys (myKeys)
import Layout (myLayoutHook)
import Managers (pbManageHook, myManageHook)

main :: IO ()
main = 
  let
      conf = ewmh xfceConfig
        { manageHook         = pbManageHook <+> myManageHook
                                            <+> manageDocks
                                            <+> manageHook xfceConfig
        , layoutHook         = avoidStruts (myLayoutHook)
        , handleEventHook    = ewmhDesktopsEventHook <+> fullscreenEventHook 
        , logHook            = ewmhDesktopsLogHook
        , borderWidth        = 2
        , focusedBorderColor = "#990000"
        , normalBorderColor  = "#444444"
        , workspaces         = ["main","mail","work","chat","5","6","7","games","music"]
        , modMask            = mod4Mask
        , keys               = myKeys
        , focusFollowsMouse  = True
        , terminal           = "qterminal"
        }
   in
    xmonad $ conf
      { startupHook       = startupHook conf >> setWMName "LG3D" -- Java app focus fix
      }
