import           XMonad

import           XMonad.Config.Desktop          (desktopConfig,
                                                 desktopLayoutModifiers)

import           XMonad.Actions.GroupNavigation (historyHook)
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.ShowText        (handleTimerEvent)

import           XMonad.Hooks.EwmhDesktops      (ewmh, fullscreenEventHook)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook       (NoUrgencyHook (..),
                                                 withUrgencyHook)

import           XMonad.Util.Run                (spawnPipe)
import           XMonad.Util.SpawnOnce          (spawnOnOnce)

import           Keys                           (myKeys)
import           Layout                         (myLayoutHook)
import           Logging                        (xmobarLogHook)
import           Managers                       (myManageHook)
import qualified Workspaces

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook
      . ewmh
      . withNavigation2DConfig def
        { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
        }
      $ desktopConfig
        { borderWidth        = 2
        , clickJustFocuses   = False
        , clientMask         = clientMask desktopConfig
        , focusFollowsMouse  = False
        , focusedBorderColor = "#ca9133"

        , handleEventHook    = handleEventHook desktopConfig
                                <+> fullscreenEventHook
                                <+> handleTimerEvent

        , handleExtraArgs    = handleExtraArgs desktopConfig
        , keys               = myKeys
        , layoutHook         = desktopLayoutModifiers myLayoutHook
        , logHook            = logHook desktopConfig
                                <+> historyHook
                                <+> xmobarLogHook xmproc
        , manageHook         = manageHook desktopConfig
                                <+> myManageHook

        , modMask            = mod4Mask
        , mouseBindings      = mouseBindings desktopConfig
        , normalBorderColor  = "#282828"
        , rootMask           = rootMask desktopConfig
        , startupHook        = startup
        , terminal           = "kitty"
        , workspaces         = Workspaces.numbered
        }

  where
    startup = do
      startupHook desktopConfig
      spawnOnOnce "2" "thunderbird"
      spawnOnOnce "3" "slack"
      spawnOnOnce "3" "signal-desktop"
      spawnOnOnce "9" "spotify"
      setWMName "LG3D" -- Java app focus fix
