{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Layout (myLayoutHook) where

import XMonad.Layout
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.Circle
-- import XMonad.Layout.ComboP
-- import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
-- import XMonad.Layout.TwoPane

myLayoutHook = ntile ||| rtile ||| mtile ||| full ||| tab ||| threeCol ||| bsp ||| Circle
  where
    rt      = defaultSpacing 4 $ ResizableTall 1 (2/100) (8/13) []
    -- normal vertical tile
    ntile   = named "[]="   $ smartBorders rt
    rtile   = named "=[]"   $ reflectHoriz $ smartBorders rt
    -- normal horizontal tile
    mtile   = named "M[]="  $ smartBorders $ Mirror rt
    -- fullscreen with tabs
    tab     = named "T"     $ noBorders $ tabbed shrinkText tabTheme
    -- two tab panes beside each other, move windows with SwapWindow
    -- tabB    = noBorders     $ tabbed shrinkText tabTheme
    -- tabtile = named "TT"    $ combineTwoP (TwoPane 0.03 0.5)
    --                                       (tabB)
    --                                       (tabB)
    --                                       (ClassName "Firefox" `Or` ClassName "Google-chrome")
    -- two layouts for gimp, tabs and tiling
    -- gimp    = named "gimp"  $ combineTwoP (TwoPane 0.03 0.15)
    --                                       (tabB) (reflectHoriz
    --                                               $ combineTwoP (TwoPane 0.03 0.2)
    --                                                 tabB        (tabB ||| Grid)
    --                                                             (Role "gimp-dock")
    --                                              )
    --                                       (Role "gimp-toolbox")
    -- fullscreen without tabs
    full    = named "[]"    $ noBorders Full

    threeCol = withBorders $ ThreeColMid 1 (2/100) (1/2)

    bsp = withBorders $ borderResize emptyBSP

    withBorders = smartBorders . defaultSpacing 4
    defaultSpacing i = spacingRaw True (uniformBorder i) True (uniformBorder i) True
    uniformBorder i = Border i i i i

    -- make renamed work more like named, which is now deprecated
    named t = renamed [Replace t]

tabTheme = def
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
