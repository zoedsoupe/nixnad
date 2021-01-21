module Zoey.Variables where

import XMonad
import qualified XMonad.StackSet as W

myFont :: String
myFont = "xft:Victor Mono Medium Nerd Font Complete Mono:size=13:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask      -- Sets modKey to command/super key

myTerminal :: String
myTerminal = "alacritty"  -- Sets default terminal

myBrowser :: String
myBrowser = "microsoft-edge-dev" -- Sets edge for tree select

myEditor :: String
myEditor = "emacsclient -c -a emacs" -- Sets emacs as editor for tree select

myBorderWith :: Dimension
myBorderWith = 2           -- Window Border

myNormColor :: String
myNormColor   = "#282c34"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
