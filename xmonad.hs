-------------------------------------------------------------------------------
-- Configuration for using xmonad inside xfce.
--
-- Author: Johannes 'wulax' Sjölund
-- Based on the work of Øyvind 'Mr.Elendig' Heggstad
--
-- Last tested with xmonad 0.13 and xfce 4.12.1
--
-- 1. Start xmonad by adding it to "Application Autostart" in xfce.
-- 2. Make sure xfwm4 is disabled from autostart, or uninstalled.
-- 3. Make sure xfdesktop is disabled from autostart, or uninstalled
--    since it may prevent xfce-panel from appearing once xmonad is started.
-------------------------------------------------------------------------------

import qualified Data.Map as M

import qualified XMonad.StackSet as W
import Control.Exception
import System.Exit

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.ComboP
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

conf = ewmh xfceConfig
        { manageHook        = pbManageHook <+> myManageHook
                                           <+> manageDocks
                                           <+> manageHook xfceConfig
        , layoutHook        = avoidStruts (myLayoutHook)
        , handleEventHook   = ewmhDesktopsEventHook <+> fullscreenEventHook 
        , borderWidth       = 1
        , focusedBorderColor= "red"
        , normalBorderColor = "#444444"
        , workspaces        = map show [1 .. 9 :: Int]
        , modMask           = mod4Mask
        , keys              = myKeys
         }
    -- where
        -- tall                = ResizableTall 1 (3/100) (1/2) []

-- Main --
main :: IO ()
main =
    xmonad $ conf
        { startupHook       = startupHook conf
                            >> setWMName "LG3D" -- Java app focus fix
        , logHook           =  ewmhDesktopsLogHook
         }

myBar = "xmobar"

-- Tabs theme --
myTabTheme = defaultTheme
    { activeColor           = "white"
    , inactiveColor         = "grey"
    , urgentColor           = "red"
    , activeBorderColor     = "grey"
    , inactiveBorderColor   = "grey"
    , activeTextColor       = "black"
    , inactiveTextColor     = "black"
    , decoHeight            = 22
    , fontName              = "xft:Liberation Sans:size=10"
    }

-- Layouts --
myLayoutHook = tile ||| rtile ||| full ||| mtile ||| gimp
  where
    rt      = ResizableTall 1 (2/100) (1/2) []
    -- normal vertical tile
    tile    = named "[]="   $ smartBorders rt
    rtile   = named "=[]"   $ reflectHoriz $ smartBorders rt
    -- normal horizontal tile
    mtile   = named "M[]="  $ smartBorders $ Mirror rt
    -- fullscreen with tabs
    tab     = named "T"     $ noBorders $ tabbed shrinkText myTabTheme
    -- two tab panes beside each other, move windows with SwapWindow
    tabB    = noBorders     $ tabbed shrinkText myTabTheme
    tabtile = named "TT"    $ combineTwoP (TwoPane 0.03 0.5)
                                          (tabB)
                                          (tabB)
                                          (ClassName "firefox")
    -- two layouts for gimp, tabs and tiling
    gimp    = named "gimp"  $ combineTwoP (TwoPane 0.03 0.15)
                                          (tabB) (reflectHoriz
                                                  $ combineTwoP (TwoPane 0.03 0.2)
                                                    tabB        (tabB ||| Grid)
                                                                (Role "gimp-dock")
                                                 )
                                          (Role "gimp-toolbox")
    -- fullscreen without tabs
    full        = named "[]"    $ noBorders Full


-- Default managers
--
-- Match a string against any one of a window's class, title, name or
-- role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

-- Match against @WM_NAME@.
name :: Query String
name = stringProperty "WM_CLASS"

-- Match against @WM_WINDOW_ROLE@.
role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

-- ManageHook --
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks ]
    , [ manageHook defaultConfig ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    , [ fmap not isDialog --> doF avoidMaster ]
    ]

{-|
# Script to easily find WM_CLASS for adding applications to the list
#! /bin/sh
exec xprop -notype \
  -f WM_NAME        8s ':\n  title =\? $0\n' \
  -f WM_CLASS       8s ':\n  appName =\? $0\n  className =\? $1\n' \
  -f WM_WINDOW_ROLE 8s ':\n  stringProperty "WM_WINDOW_ROLE" =\? $0\n' \
  WM_NAME WM_CLASS WM_WINDOW_ROLE \
  ${1+"$@"}
-}
myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions]
    where myActions =
            [ ("Xfrun4"                         , doFloat)
            , ("Xfce4-notifyd"                  , doIgnore)
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
            ]

-- Helpers --
-- avoidMaster:  Avoid the master window, but otherwise manage new windows normally
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    otherwise           -> c

-- Keyboard --
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,                xK_Return   ), spawn "qterminal")
    , ((modMask,                xK_o        ), spawn "xfrun4")
    , ((modMask,                xK_f        ), spawn "pcmanfm")
    , ((modMask .|. shiftMask,  xK_c        ), spawn "xkill")
    , ((modMask,                xK_c        ), kill)
    , ((modMask,                xK_b        ), sendMessage ToggleStruts)

    -- layouts
    , ((modMask,                xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,  xK_space    ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,                xK_t        ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,                xK_r        ), refresh)

    -- focus
    , ((modMask,                xK_Tab      ), windows W.focusDown)
    , ((modMask,                xK_j        ), windows W.focusDown)
    , ((modMask,                xK_k        ), windows W.focusUp)
    , ((modMask,                xK_m        ), windows W.focusMaster)
    , ((modMask,                xK_Right    ), nextWS)
    , ((modMask,                xK_Left     ), prevWS)
    , ((modMask .|. shiftMask,  xK_Right    ), shiftToNext >> nextWS)
    , ((modMask .|. shiftMask,  xK_Left     ), shiftToPrev >> prevWS)

    -- swapping
    , ((modMask .|. shiftMask,  xK_Return   ), windows W.swapMaster)
    , ((modMask .|. shiftMask,  xK_j        ), windows W.swapDown)
    , ((modMask .|. shiftMask,  xK_k        ), windows W.swapUp)
    , ((modMask,                xK_s        ), sendMessage $ SwapWindow)

    -- increase or decrease number of windows in the master area
    , ((modMask,                xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                xK_period   ), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,                xK_h        ), sendMessage Shrink)
    , ((modMask,                xK_l        ), sendMessage Expand)
    , ((modMask .|. shiftMask,  xK_h        ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask,  xK_l        ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask,  xK_q        ), spawn "xfce4-session-logout")
    , ((mod1Mask .|. shiftMask, xK_q        ), spawn "xscreensaver-command --lock")

    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)

    -- ungrab mouse cursor from applications which can grab it (games)
    , ((modMask,                xK_i        ), spawn "xdotool key XF86Ungrab")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
