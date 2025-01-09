{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Custom.MyLayouts where


import Custom.MyDecorations
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BoringWindows
import XMonad.Layout.Column
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.Renamed as XLR
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

import XMonad.Layout.FixedAspectRatio
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Gaps

mySpacing i = spacingRaw False (Border 5 5 10 10) True (Border i i i i) True


tabs =
  renamed [XLR.Replace "Tabs"] $
    avoidStruts $
      tabbed
        shrinkText
        myTabConfig

tall =
  renamed [XLR.Replace "Tall"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 $
              ResizableTall nmaster delta ratio []
  where
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100


column =
  renamed [XLR.Replace "Column"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 $
              Column 1.0

accordion =
  renamed [XLR.Replace "Accordion"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 Accordion

bsp =
  renamed [XLR.Replace "BSP"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 emptyBSP


full = renamed [XLR.Replace "Full"] $ noBorders Full

sf = renamed [XLR.Replace "Float"] $ noBorders simplestFloat

-- myLayout = boringWindows (ifWider 1080 (tall ||| bsp) (column ||| accordion) ||| tabs ||| full)

myLayout = boringWindows (tall ||| bsp ||| column ||| accordion ||| tabs ||| full)

-- STUDY: Check if XMonad.Layout.showWName allows for custom modification
-- of the text provided.
myLayoutHook =
  showWName' myShowWNameConfig $
    smartBorders $
      mkToggle
        (NOBORDERS ?? FULL ?? EOT)
        myLayout



-- From here it's ignored but it would be nice to play with this setting instead
-- The layout hook
myLayoutHook2 = renamed [KeepWordsRight 1]
               . avoidStruts
               . fixedAspectRatio (0.5, 0.5)
               . layoutHintsWithPlacement (0.5, 0.5)
               . spacingRaw False (Border 0 0 0 0) True (Border 0 0 0 0) True -- between windows
               . gaps [(U, 2), (R, 2), (L, 2), (D, 2)] -- along the screen, excluding docks
               . mkToggle (single NBFULL) -- toggle full screen
               . smartBorders
               $ tiled
                   ||| mtiled
                   ||| full
          where
            tiled = renamed [XLR.Replace "T "] $ ResizableTall 1 (3 / 100) (5 / 8) []
            mtiled = renamed [XLR.Replace "MT"] $ Mirror tiled
            full = renamed [XLR.Replace "F "] Full
