module Matthew.Hooks where

-- Base
import XMonad
import Data.Ratio
import Data.Monoid (All)
import System.IO (hPutStrLn)
import GHC.IO.Handle.Types (Handle)

-- Hooks
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat, isDialog)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

-- Utils
import XMonad.Util.SpawnOnce

-- Layout
import qualified XMonad.StackSet as W
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.LimitWindows (limitSelect)
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenFull)

-- Personal
import Matthew.Variables (windowCount)
import Matthew.Layout (MyLayout, layouts)

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> fullscreenEventHook

-- Layout hook - all quite standard, note the use of limitSelect (I
-- have a key-binding to limit the windows defined below, which is
-- pretty handy)
myLayoutHook :: MyLayout Window
myLayoutHook =
  avoidStruts
    . smartBorders
    . fullscreenFull
    . toggleLayouts Full
    . limitSelect 1 5
    $ layouts

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn h
                        , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#d0d0d0" "" . shorten 80     -- Title of active window in xmobar
                        , ppSep =  "<fc=#9AEDFE> : </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }

myStartupHook :: X ()
myStartupHook = do
        spawnOnce "clipmenud &"
        spawnOnce "picom --experimental-backends &"
        spawnOnce "/usr/bin/emacs --daemon &"
        spawnOnce "feh --bg-fill --randomize ~/pics/wallpapers &"

pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks                                      ]
    , [ manageHook def                                   ]
    , [ isDialog     --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)) ]
    , [ isFullscreen --> doF W.focusDown <+> doFullFloat ]
    ]

myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions ]
    where myActions = [ ("spotify"   , doShift "9" )
                      , ("discord"   , doShift "8" )
                      ]

matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

name :: Query String
name = stringProperty "WM_NAME"

role :: Query String
role = stringProperty "WM_ROLE"
