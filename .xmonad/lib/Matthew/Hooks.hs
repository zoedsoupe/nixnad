module Matthew.Hooks where

import XMonad
import Data.Ratio
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (isFullscreen,  doFullFloat, doRectFloat, isDialog) 

myStartupHook :: X ()
myStartupHook = do
        spawnOnce "clipmenud &"
        spawnOnce "picom --experimental-backends &"
        spawnOnce "/usr/bin/emacs --daemon &"
        spawnOnce "feh --bg-fill --randomize ~/Pics/wallpapers &"

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
