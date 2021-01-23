module Zoey.Keybindings where

-- Base
import XMonad
import System.Exit (exitSuccess)

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.Submap (submap)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)

-- Utils
import XMonad.Util.Paste (sendKey)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (safeSpawn, safeSpawnProg)

-- Layout
import XMonad.Layout.ResizableTile
import XMonad.Layout.LimitWindows (setLimit)
import qualified XMonad.StackSet as W

-- Personal
import Zoey.TreeSelect
import Zoey.Variables (myTerminal)

-- Function to generate a modal keymap
modal :: XConfig a -> [(String, X ())] -> X ()
modal cfg bindings = modalMap
  where
    exitKeys = ["<Space>", "<Return>", "<Escape>"]
    modalMap = submap . mkKeymap cfg $ map f bindings ++ map g exitKeys
    f (k, a) = (k, a >> modalMap)
    g k = (k, pure ())

 -- set the backlight - I like 0 to yield 100%
setBacklight :: Int -> X ()
setBacklight n = spawn $ "xbacklight -set " <> show (f n)
  where
    f 0 = 100
    f i = 1 + 11 * (i - 1)   

myKeys :: XConfig a -> [(String, X ())] 
myKeys cfg =
  let
    modal' = modal cfg
   in
    [
      -- Workspaces
      ("M-<KP_Add>", moveTo Next (WSIs $ return ((/= "NSP") . W.tag)))                                -- Go to next workspace
    , ("M-<KP_Subtract>", moveTo Prev (WSIs $ return ((/= "NSP") . W.tag)))                           -- Go to previous workspace
    ]
    -- set number of windows in workspace
    ++ map (\n -> ("M-; " <> show n, setLimit $ n + 1)) [0 .. 9]
    -- Set backlight brightness
    ++ map (\n -> ("M-S-b " <> show n, setBacklight n))
       [0..9]
    ++ [
    -- Managing Windows
    
         ("M-q", kill)                           -- Kill the currently focused client
       , ("M-S-a", killAll)                         -- Kill all the windows on current workspace
       
    -- Floating windows 
       , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
       , ("M-S-<Delete>", sinkAll)                  -- ush ALL floating windows back to tile.

    -- Windows navigation
       , ("M-m", windows W.focusMaster)             -- Move focus to the master window
       , ("M-j", windows W.focusDown)
       , ("M-k", windows W.focusUp)
       , ("M-S-j", windows W.swapDown)              -- Swap the focused window with the next window
       , ("M-S-k", windows W.swapUp)                -- Swap the focused window with the prev window
       , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
       , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
        
    -- Window resizing
       , ("M-h", sendMessage Shrink)
       , ("M-l", sendMessage Expand)
       , ("M1-S-j", sendMessage MirrorShrink)
       , ("M1-S-k", sendMessage MirrorExpand)

    -- Modal modeto move floating windows 
       , ( "M-n"
         , let n = fromIntegral (40 :: Int)
           in  modal'
                 [ ("h"  , withFocused (keysMoveWindow (-n, 0)))
                 , ("l"  , withFocused (keysMoveWindow (n, 0)))
                 , ("k"  , withFocused (keysMoveWindow (0, -n)))
                 , ("j"  , withFocused (keysMoveWindow (0, n)))
                 , ("S-h", withFocused (keysResizeWindow (-n, 0) (0, 0)))
                 , ("S-l", withFocused (keysResizeWindow (n, 0) (0, 0)))
                 , ("S-j", withFocused (keysResizeWindow (0, n) (0, 0)))
                 , ("S-k", withFocused (keysResizeWindow (0, -n) (0, 0)))
                 ]
         )
       ]
    ++ [
    -- Managing Applications

       -- Xmonad
         ("M-C-r", safeSpawn "xmonad" ["--recompile"])      -- Recompiles xmonad
       , ("M-S-r", safeSpawn "xmonad" ["--restart"])        -- Restarts xmonad
       , ("M-S-q", io exitSuccess)                          -- Quits xmonad
       
       -- Open Terminal
       , ("M-<Return>", safeSpawnProg myTerminal)

       -- Tree Select
       , ("C-t t", treeselectAction tsDefaultConfig)

       -- change tab in an underlying window, very convenient in
       -- Edge where Vimium often does not work when a search
       -- dialogue is open and where the standard shortcut "C-<Tab>"
       -- is awkward
       , ( "M-<Tab>"
         , modal'
           [ ("k", sendKey controlMask xK_Tab)
           , ("j", sendKey (controlMask .|. shiftMask) xK_Tab)
           ]
         )

       , ("M-e"
         , modal'
           [ ("e", spawn "emacsclient -c")
           , ("b", spawn "emacsclient -c --eval '(ibuffer)'")
           , ("v", spawn "emacsclient -c --eval '(+vterm/here nil)'")
           ]
         )

    --- My Applications
       , ("M-t", spawn (myTerminal ++ " -e ytop"))
       , ("M-w", safeSpawnProg "microsoft-edge-dev")
       , ("M-m", safeSpawnProg "spotify")
       , ("M-S-m", safeSpawnProg "discord")
       , ("M-c", safeSpawnProg "clipmenu")
       , ("M-s", safeSpawnProg "screenkey")
       , ("M-S-s", safeSpawn "killall" ["screenkey"])
       
    --- System
       , ("M-x", safeSpawnProg "reboot")
       , ("M-S-x", safeSpawnProg "poweroff")
       , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
       , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
       , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
       , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
       , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
       , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ]
