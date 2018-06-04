{-# LANGUAGE FlexibleContexts #-}
-- Ondřej Súkup xmonad config ...
-- main import
import XMonad
import XMonad.Config.Xfce
-- xmonad hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
--xmonad actions
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
--xmonad utils
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Themes
--xmonad layouts
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
-- import xmonad promt
import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
-- qualified imports of Data and Stack
import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- general import
import Graphics.X11.ExtraTypes.XF86
import Control.Applicative
------------------------------------------------------------------------
promptConfig =
  defaultXPConfig {font = "xft:Source Code Pro:pixelsize=12"
                  ,borderColor = "#1e2320"
                  ,height = 18
                  ,position = Top}
------------------------------------------------------------------------
myWorkspaces :: [WorkspaceId]
myWorkspaces =
  ["con","web","irc","email"] ++
  map show [5 .. 9]
------------------------------------------------------------------------
-- add mouse buttons
button8 = 8 :: Button
button9 = 9 :: Button
------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =
  smartBorders $
  layoutHints $
  onWorkspace "con"
              (tab ||| tiled ||| mtiled) $
  onWorkspaces ["web","irc"]
               full $
  full ||| tab ||| tiled ||| mtiled
  where
        -- default tiling algorithm partitions the screen into two panes
        tiled = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio =
          toRational
            (2 /
             (1 +
              sqrt 5 :: Double))
        -- Percent of screen to increment by when resizing panes
        delta = 5 / 100
        -- tab is tabbed
        tab =
          tabbed shrinkText (theme smallClean)
        -- full is Full
        full =
          (fullscreenFloat . fullscreenFull) Full
        -- mtiled is mirrortiled
        mtiled = Mirror tiled
------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook =
  composeAll $
  [isDialog --> doFloat] ++
  [appName =? r --> doIgnore | r <- myIgnores] ++
  -- auto-float certain windows
  [className =? c --> doCenterFloat | c <- myCenFloats] ++
  [title =? t --> doFloat | t <- myFloat] ++
  -- send certain windows to certain workspaces
  [appName =? r --> doShift wsp | (r,wsp) <- myWorkspaceMove]
  -- fulscreen windows to fullfloating
  ++
  [isFullscreen --> doFullFloat] ++
  -- unmanage docks such as gnome-panel and dzen
  [fullscreenManageHook
  ,scratchpadManageHookDefault]
  where
        -- windows to operate
        myIgnores =
          ["desktop","kdesktop","desktop_window","stalonetray"]
        myCenFloats =
          ["Steam","steam","vlc","Vlc","mpv"]
        myFloat = ["Hangouts"]
        myWorkspaceMove =
          [("Google-chrome-stable","web")
          ,("xfce4-terminal","con")
          ,("weechat","irc")
          ,("Steam","steam")
          ,("steam","steam")
          ,("Navigator","web")
          ,("Hexchat","irc")
          ,("hexchat","irc")
          ,("Thunderbird","email")
          ,("Mail","email")]
------------------------------------------------------------------------
-- Event handling
--
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> hintsEventHook
------------------------------------------------------------------------
-- myStartupHook
myStartupHook =
  do setDefaultCursor xC_left_ptr
     spawnOnce "google-chrome-stable"
     spawnOnce "urxvtc"
     spawnOnce "urxvtc -name weechat -e weechat"
     spawnOnce "stalonetray"
------------------------------------------------------------------------
-- Urgency Hook:
--
-- Use libnotify notifications when the X11 urgent hint is set
data LibNotifyUrgencyHook =
  LibNotifyUrgencyHook
  deriving (Read,Show)
instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w =
    do name <- getName w
       wins <- gets windowset
       whenJust (W.findTag w wins)
                (flash name)
    where flash name index = spawn $ "notify-send " ++ "'Workspace " ++ index ++
                                                                        "' " ++
                                                                        "'Activity in: " ++
                                                                        show name ++
                                                                        "' "
------------------------------------------------------------------------
--
-- |
-- Helper function which provides ToggleStruts keybinding
--
toggleStrutsKey :: XConfig t -> (KeyMask,KeySym)
toggleStrutsKey XConfig{modMask = modm} =
  (modm,xK_b)
------------------------------------------------------------------------
-- xmobar from XMonad.Hooks.DynamicLog with scratchpad filter
--
myXmobar :: LayoutClass l Window
         => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar =
  statusBar "xmobar"
            xmobarPP {ppSort = (. scratchpadFilterOutWorkspace) Control.Applicative.<$>
                               ppSort defaultPP}
            toggleStrutsKey
-----------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = xmonad =<< myXmobar defaults
------------------------------------------------------------------------
myUrgencyHook =
  withUrgencyHook LibNotifyUrgencyHook
defaults =
  myUrgencyHook $
  ewmh $
  defaultConfig {terminal = "urxvtc"  -- unicode rxvt as client for urxvtd started in .xsession file
                ,borderWidth = 2
                ,modMask = mod4Mask
                ,workspaces = myWorkspaces
                ,layoutHook = myLayout
                ,manageHook = myManageHook
                ,handleEventHook = myEventHook
                ,startupHook = myStartupHook} `removeKeys`
  [(mod4Mask .|. m,k) | (m,k) <- zip [0,shiftMask]
                                     [xK_w,xK_e,xK_r,xK_p]] `additionalKeys`
  [((mod4Mask,xK_g),goToSelected defaultGSConfig) -- Gridselect
  ,((mod4Mask,xK_Print),spawn "scrot '%F-%H-%M-%S.png' -e 'mv $f ~/Shot/'") -- screenshot
  ,((mod4Mask,xK_s),scratchpadSpawnAction defaults) -- scratchpad
  ,((mod4Mask .|. controlMask,xK_p)
   ,submap . M.fromList $ -- add submap Ctrl+Win+P,key
    [((0,xK_q),spawn "urxvtc -name weechat -e weechat")
    ,((0,xK_w),spawn "google-chrome-stable")
    ,((0,xK_e),spawn "urxvtc -name EDIT -e vim")
    ,((0,xK_r),spawn "steam")])
  ,((mod4Mask,xK_p),shellPrompt promptConfig)
  ,((mod4Mask .|. shiftMask,xK_p),passPrompt promptConfig)
  ,((mod4Mask,xK_l),spawn "i3lock -i ./Wallpaper/lock.png")
  ,((0,xF86XK_AudioMute),spawn "pulseaudio-ctl mute")
  ,((0,xF86XK_AudioRaiseVolume),spawn "pulseaudio-ctl up")
  ,((0,xF86XK_AudioLowerVolume),spawn "pulseaudio-ctl down")] `additionalMouseBindings`
  [((0,button8),const prevWS) -- cycle Workspace up
  ,((0,button9),const nextWS) -- cycle Workspace down
   ]
