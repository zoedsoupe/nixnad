module Matthew.Layout where

-- Hooks
import XMonad.Hooks.ManageDocks (AvoidStruts)

-- Layout
import XMonad.Layout (Full)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.NoBorders (SmartBorder)
import XMonad.Layout.LimitWindows (Selection)
import XMonad.Layout.Fullscreen (FullscreenFull)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.ToggleLayouts (ToggleLayouts)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Reflect (Reflect, reflectHoriz)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.LayoutCombinators (NewSelect, (|||))

type MyLayout
   = ModifiedLayout AvoidStruts (ModifiedLayout SmartBorder (ModifiedLayout FullscreenFull (ToggleLayouts Full (ModifiedLayout Selection (NewSelect ResizableTall (NewSelect (ModifiedLayout Reflect ResizableTall) (NewSelect TwoPane (NewSelect Grid (NewSelect ThreeCol ThreeCol)))))))))

-- The type of my layouts - not sure there is an easier way to express this
type MyLayouts a
   = NewSelect ResizableTall (NewSelect (ModifiedLayout Reflect ResizableTall) (NewSelect TwoPane (NewSelect Grid (NewSelect ThreeCol ThreeCol)))) a

-- Layouts
layouts :: MyLayouts a
layouts =
  tall ||| reflectedTall ||| twopane ||| grid ||| threecol ||| threecolmid
 where
  tall          = ResizableTall 1 0.03 (φ / (1 + φ)) []
  reflectedTall = reflectHoriz tall
  threecol      = ThreeCol 1 (3 / 100) (1 / 2)
  threecolmid   = ThreeColMid 1 (3 / 100) (1 / 2)
  twopane       = TwoPane 0.03 (1 / φ)
  grid          = Grid
  φ             = realToFrac ((1.0 + sqrt 5) / 2.0 :: Double)
