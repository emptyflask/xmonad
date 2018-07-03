module Keys (myKeys) where

import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import qualified XMonad.Actions.Search as Search
import qualified XMonad.Actions.Submap as Submap
import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks

import XMonad.Layout.ComboP (SwapWindow(..))
import XMonad.Layout.ResizableTile (MirrorResize(..))

import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh

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
    , ((modMask .|. shiftMask,  xK_r        ), refresh)

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

    -- search (mod-s [g,h,w,y])
    , ((modMask,                xK_s        ), promptSearch)
    , ((modMask .|. shiftMask,  xK_s        ), selectSearch)

    -- quit, or restart
    , ((modMask  .|. shiftMask, xK_q        ), spawn "xfce4-session-logout")
    , ((mod1Mask .|. shiftMask, xK_q        ), spawn "xscreensaver-command --lock")

    -- Restart xmonad
    , ((modMask,                xK_q        ), restart "xmonad" True)

    -- ungrab mouse cursor from applications which can grab it
    , ((modMask,                xK_i        ), spawn "xdotool key XF86Ungrab")

    , ((modMask,                xK_F1       ), manPrompt Prompt.def)
    , ((modMask,                xK_F2       ), sshPrompt Prompt.def)

    ] ++ workspaceKeys ++ screenKeys

 where
    promptSearch = Submap.submap . searchEngineMap $ Search.promptSearch Prompt.def
    selectSearch = Submap.submap . searchEngineMap $ Search.selectSearch
    searchEngineMap method = M.fromList $
      [ ((0, xK_g), method Search.google)
      , ((0, xK_h), method Search.hoogle)
      , ((0, xK_w), method Search.wikipedia)
      , ((0, xK_y), method Search.youtube)
      ]

    -- mod-[1..9]       Switch to workspace N
    -- mod-shift-[1..9] Move client to workspace N
    workspaceKeys =
      [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

    -- mod-{w,e,r}       Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} Move client to screen 1, 2, or 3
    screenKeys =
      [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
