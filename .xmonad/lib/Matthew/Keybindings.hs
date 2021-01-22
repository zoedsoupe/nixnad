module Zoey.Keybindings where

import XMonad
import XMonad.Actions.CycleWS
import System.Exit (exitSuccess)
import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Util.Run (safeSpawn, safeSpawnProg)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.GridSelect (bringSelected, goToSelected)

import Zoey.GridSelect
import Zoey.TreeSelect
import Zoey.Variables (myTerminal)

myKeys :: [(String, X())]
myKeys =
    --- Xmonad
        [ ("M-C-r", safeSpawn "xmonad" ["--recompile"])      -- Recompiles xmonad
        , ("M-S-r", safeSpawn "xmonad" ["--restart"])        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad
    
    --- Windows
        , ("M-q", kill)                           -- Kill the currently focused client
        , ("M-S-a", killAll)                         -- Kill all the windows on current workspace

    --- Floating windows
        , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
        , ("M-S-<Delete>", sinkAll)                  -- ush ALL floating windows back to tile.

    --- Windows navigation
        , ("M-m", windows W.focusMaster)             -- Move focus to the master window
        , ("M-<Left>", windows W.focusDown)
        , ("M-<Right>", windows W.focusUp)
        , ("M-S-j", windows W.swapDown)              -- Swap the focused window with the next window
        , ("M-S-k", windows W.swapUp)                -- Swap the focused window with the prev window
        , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
        
    --- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Exoand vert window width       

    --- Workspaces
        , ("M-<KP_Add>", moveTo Next nonNSP)                                -- Go to next workspace
        , ("M-<KP_Subtract>", moveTo Prev nonNSP)                           -- Go to previous workspace
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next workspace
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to previous workspace

    --- Open Terminal
        , ("M-<Return>", safeSpawnProg myTerminal)

     -- Grid Select (CTR-g followed by a key)
        , ("C-g g", spawnSelected' myAppGrid)                 -- grid select favorite apps
        , ("C-g t", goToSelected $ mygridConfig myColorizer)  -- goto selected window
        , ("C-g b", bringSelected $ mygridConfig myColorizer) -- bring selected window

    -- Tree Select
        , ("C-t t", treeselectAction tsDefaultConfig)       

    --- My Applications
        , ("M-t", spawn (myTerminal ++ " -e ytop"))
        , ("M-w", safeSpawnProg "microsoft-edge-dev")
        , ("M-v", spawn (myTerminal ++ " -e nvim"))
        , ("M-m", safeSpawnProg "spotify")
        , ("M-S-m", safeSpawnProg "discord")
        , ("M-c", safeSpawnProg "clipmenu")
        , ("M-s", safeSpawnProg "screenkey")
        , ("M-S-s", safeSpawn "killall" ["screenkey"])
        
    --- Emacs (CTRL-e followed by a key)
        , ("C-e e", spawn "emacsclient -c -a 'emacs'")                            -- start emacs
        , ("C-e b", spawn "emacsclient -c -a 'emacs' --eval '(ibuffer)'")         -- list emacs buffers
        , ("C-e v", spawn "emacsclient -c -a 'emacs' --eval '(+vterm/here nil)'") -- vterm within emacs

    --- System
        , ("M-x", safeSpawnProg "reboot")
        , ("M-S-x", safeSpawnProg "poweroff")
        , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
        , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
        , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
