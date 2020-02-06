module Managers (myManageHook, scratchpads) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.SpawnOn
import XMonad.Util.NamedScratchpad


scratchpads :: [NamedScratchpad]
scratchpads = [ NS "calc" spawnCalc findCalc manageCalc
              , NS "htop" spawnHtop findHtop manageHtop
              , NS "zeal" spawnZeal findZeal manageZeal
              ]
    where
      spawnCalc  = "qalculate-gtk"
      findCalc   = className =? "Qalculate-gtk"
      manageCalc = doCenterFloat

      spawnHtop  = "kitty --class=htop htop"
      findHtop   = className =? "htop"
      manageHtop = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

      spawnZeal  = "zeal"
      findZeal   = className =? "Zeal"
      manageZeal = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)


myManageHook :: ManageHook
myManageHook = composeAll $
  [ manageDocks
  , manageSpawn
  , manageHook def
  , namedScratchpadManageHook scratchpads
  , isDialog --> doCenterFloat
  , isFullscreen --> doFullFloat
  , fmap not isDialog --> doF avoidMaster
  ] ++ [ matchAny v --> a | (v,a) <- myActions ]

    where

      {-|
        Script to easily find WM_CLASS for adding applications to the list
        #!/bin/sh
        exec xprop -notype \
          -f WM_NAME        8s ':\n  title =\? $0\n' \
          -f WM_CLASS       8s ':\n  appName =\? $0\n  className =\? $1\n' \
          -f WM_WINDOW_ROLE 8s ':\n  stringProperty "WM_WINDOW_ROLE" =\? $0\n' \
          WM_NAME WM_CLASS WM_WINDOW_ROLE \
          ${1+"$@"}
      -}

      myActions = [ ("Xfrun4"                         , doFloat)
                  , ("Xfce4-notifyd"                  , doIgnore)
                  , ("Xfce4-appfinder"                , doFloat)
                  , ("rofi"                           , doIgnore)
                  , ("MPlayer"                        , doFloat)
                  , ("mpv"                            , doFloat)
                  , ("Oracle VM VirtualBox Manager"   , doShift "8")
                  , ("VirtualBox"                     , doShift "8")
                  , ("animation-SpriteTestWindow"     , doFloat)
                  , ("gimp-image-window"              , (ask >>= doF . W.sink))
                  , ("gimp-toolbox"                   , (ask >>= doF . W.sink))
                  , ("gimp-dock"                      , (ask >>= doF . W.sink))
                  , ("gimp-image-new"                 , doFloat)
                  , ("gimp-toolbox-color-dialog"      , doFloat)
                  , ("gimp-layer-new"                 , doFloat)
                  , ("gimp-vectors-edit"              , doFloat)
                  , ("gimp-levels-tool"               , doFloat)
                  , ("preferences"                    , doFloat)
                  , ("gimp-keyboard-shortcuts-dialog" , doFloat)
                  , ("gimp-modules"                   , doFloat)
                  , ("unit-editor"                    , doFloat)
                  , ("screenshot"                     , doFloat)
                  , ("gimp-message-dialog"            , doFloat)
                  , ("gimp-tip-of-the-day"            , doFloat)
                  , ("plugin-browser"                 , doFloat)
                  , ("procedure-browser"              , doFloat)
                  , ("gimp-display-filters"           , doFloat)
                  , ("gimp-color-selector"            , doFloat)
                  , ("gimp-file-open-location"        , doFloat)
                  , ("gimp-color-balance-tool"        , doFloat)
                  , ("gimp-hue-saturation-tool"       , doFloat)
                  , ("gimp-colorize-tool"             , doFloat)
                  , ("gimp-brightness-contrast-tool"  , doFloat)
                  , ("gimp-threshold-tool"            , doFloat)
                  , ("gimp-curves-tool"               , doFloat)
                  , ("gimp-posterize-tool"            , doFloat)
                  , ("gimp-desaturate-tool"           , doFloat)
                  , ("gimp-scale-tool"                , doFloat)
                  , ("gimp-shear-tool"                , doFloat)
                  , ("gimp-perspective-tool"          , doFloat)
                  , ("gimp-rotate-tool"               , doFloat)
                  , ("gimp-open-location"             , doFloat)
                  , ("gimp-file-open"                 , doFloat)
                  , ("animation-playbac"              , doFloat)
                  , ("gimp-file-save"                 , doFloat)
                  , ("file-jpeg"                      , doFloat)
                  , ("Wrapper-2.0"                    , doFloat)
                  ]

-- Match a string against any one of a window's class, title, name or role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, wmName, role]
  where
    -- Match against @WM_NAME@.
    wmName :: Query String
    wmName = stringProperty "WM_CLASS"

    -- Match against @WM_WINDOW_ROLE@.
    role :: Query String
    role = stringProperty "WM_WINDOW_ROLE"


-- Avoid the master window, but otherwise manage new windows normally
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    -- W.Stack t [] (r:rs) -> W.Stack t [r] rs
    W.Stack t [] (r:rs) -> W.Stack r [] (t:rs)
    _ -> c
