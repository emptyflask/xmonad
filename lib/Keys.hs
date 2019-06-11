module Keys (myKeys) where

import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import qualified XMonad.Actions.GroupNavigation as GN
import qualified XMonad.Actions.Navigation2D as N2D
import qualified XMonad.Actions.Search as Search
import qualified XMonad.Actions.ShowText as T
import qualified XMonad.Actions.Submap as Submap
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks

import XMonad.Layout.ComboP (SwapWindow(..))
import XMonad.Layout.ResizableTile (MirrorResize(..))
import XMonad.Layout.Spacing

import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh

import Graphics.X11.ExtraTypes.XF86
import System.Exit

-- Keyboard --
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((mod,                xK_Return   ), spawn "kitty")
    , ((mod,                xK_o        ), spawn "xfrun4")
    , ((mod,                xK_f        ), spawn "pcmanfm")
    , ((mod .|. shift,      xK_c        ), kill)

    -- layouts
    , ((mod,                xK_b        ), sendMessage ToggleStruts)
    , ((mod,                xK_equal    ), decScreenWindowSpacing 2)
    , ((mod .|. shift,      xK_equal    ), setScreenWindowSpacing 0)
    , ((mod,                xK_minus    ), incScreenWindowSpacing 2)
    , ((mod .|. shift,      xK_minus    ), setScreenWindowSpacing 20)
    , ((mod,                xK_space    ), sendMessage NextLayout)
    , ((mod .|. shift,      xK_space    ), flash "Reset Layout" >> setLayout (XMonad.layoutHook conf))

    -- floating layer stuff
    , ((mod,                xK_t        ), withFocused $ windows . W.sink)
    , ((mod .|. shift,      xK_t        ), sinkAll)

    -- refresh
    , ((mod,                xK_F5       ), flash "Refresh" >> refresh)

    -- focus
    , ((alt,                xK_Tab      ), GN.nextMatch GN.History (return True))
    , ((mod,                xK_Tab      ), moveTo Next NonEmptyWS)
    , ((mod .|. shift,      xK_Tab      ), moveTo Prev NonEmptyWS)
    , ((mod,                xK_j        ), windows W.focusDown)
    , ((mod,                xK_k        ), windows W.focusUp)
    , ((mod,                xK_m        ), windows W.focusMaster)
    , ((mod,                xK_Right    ), flash "->" >> nextWS)
    , ((mod,                xK_Left     ), flash "<-" >> prevWS)
    , ((mod .|. shift,      xK_Right    ), flash "Move ->" >> shiftToNext >> nextWS)
    , ((mod .|. shift,      xK_Left     ), flash "<- Move" >> shiftToPrev >> prevWS)

    -- Directional navigation of windows
    , ((mod .|. alt,        xK_Right    ), N2D.windowGo R False)
    , ((mod .|. alt,        xK_Left     ), N2D.windowGo L False)
    , ((mod .|. alt,        xK_Up       ), N2D.windowGo U False)
    , ((mod .|. alt,        xK_Down     ), N2D.windowGo D False)

    -- swapping
    , ((mod .|. shift,      xK_Return   ), windows W.swapMaster)
    , ((mod .|. shift,      xK_j        ), windows W.swapDown)
    , ((mod .|. shift,      xK_k        ), windows W.swapUp)
    -- , ((mod,                xK_s        ), sendMessage $ SwapWindow)

    -- increase or decrease number of windows in the master area
    , ((mod,                xK_comma    ), sendMessage (IncMasterN 1))
    , ((mod,                xK_period   ), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((mod,                xK_h        ), sendMessage Shrink)
    , ((mod,                xK_l        ), sendMessage Expand)
    , ((mod .|. shift,      xK_h        ), sendMessage MirrorShrink)
    , ((mod .|. shift,      xK_l        ), sendMessage MirrorExpand)

    -- search (mod-s [g,h,w,y])
    , ((mod,                xK_s        ), promptSearch)
    , ((mod .|. shift,      xK_s        ), selectSearch)

    -- quit, or restart
    -- , ((mod .|. shift,      xK_q        ), spawn "xfce4-session-logout")
    , ((mod .|. shift,      xK_q        ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((mod .|. shift,      xK_l        ), spawn "systemctl suspend")
    -- , ((mod .|. alt,        xK_l        ), spawn "xautolock -locknow")

    -- Restart xmonad
    , ((mod,                xK_q        ), restart "xmonad" True)

    -- ungrab mouse cursor from applications which can grab it
    , ((mod,                xK_i        ), spawn "xdotool key XF86Ungrab")

    -- open man pages / ssh consoles
    , ((mod,                xK_F1       ), manPrompt Prompt.def)
    , ((mod,                xK_F2       ), sshPrompt Prompt.def)

    -- open rofi
    , ((alt,                xK_space    ), spawn "rofi -plugin-path /usr/local/lib/rofi -show combi")
    , ((ctrl .|. alt,       xK_c        ), spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -theme oxide -width 900 -lines 15")

    , ((mod .|. shift,      xK_slash    ), helpCommand) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)

    ] ++ workspaceKeys ++ screenKeys ++ mediaKeys

 where
    mod   = modMask
    ctrl  = controlMask
    alt   = mod1Mask
    shift = shiftMask

    promptSearch :: X ()
    promptSearch = do flash "Search ?"
                      Submap.submap . searchEngineMap $ Search.promptSearch Prompt.def

    selectSearch :: X ()
    selectSearch = do flash "Select Search ?"
                      Submap.submap . searchEngineMap $ Search.selectSearch

    searchEngineMap :: (Search.SearchEngine -> X ()) -> M.Map (KeyMask, KeySym) (X ())
    searchEngineMap method = M.fromList $
      [ ((0, xK_d), method Search.dictionary)
      , ((0, xK_g), method Search.google)
      , ((0, xK_h), method Search.hoogle)
      , ((0, xK_i), method Search.images)
      , ((0, xK_m), method Search.maps)
      , ((0, xK_w), method Search.wikipedia)
      , ((0, xK_y), method Search.youtube)
      ]

    flash :: String -> X ()
    flash text =
      T.flashText def 1 (" " ++ text ++ " ")

    -- mod-[1..9]       Switch to workspace N
    -- mod-shift-[1..9] Move client to workspace N
    workspaceKeys :: [((KeyMask, KeySym), X ())]
    workspaceKeys =
      [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (cycle $ XMonad.workspaces conf) $ numKeys ++ numpadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
        where
          numKeys    = [xK_1 .. xK_9]
          numpadKeys = [xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down,
                        xK_KP_Left, xK_KP_Begin, xK_KP_Right,
                        xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up]

    -- mod-{w,e,r}       Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} Move client to screen 1, 2, or 3
    screenKeys :: [((KeyMask, KeySym), X ())]
    screenKeys =
      [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]
      ]

    mediaKeys :: [((KeyMask, KeySym), X ())]
    mediaKeys =
      [ ((0 , xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
      , ((0 , xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
      , ((0 , xF86XK_AudioMute),        spawn "pactl set-sink-mute   @DEFAULT_SINK@ toggle")
      ]

    helpCommand :: X ()
    helpCommand = spawn ("echo \"" ++ help ++ "\" | xmessage -file -")

help :: String
help = unlines ["The modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "",
    "-- Workspaces & screens",
    "mod-[1..9]         Switch to workSpace N",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
