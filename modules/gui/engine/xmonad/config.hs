---  _ __ ___   __| |___ _ __
--- | '_ ` _ \ / _` / __| '_ \
--- | | | | | | (_| \__ \ |_) |
--- |_| |_| |_|\__,_|___/ .__/
---                     |_|

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.Submap (submap)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)

-- Base
import XMonad hiding ((|||))
import Data.Ratio
import Data.Monoid (All)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import GHC.IO.Handle.Types (Handle)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat, isDialog)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (Direction2D(..), AvoidStruts, manageDocks, avoidStruts, docks)

-- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Paste (sendKey)
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)
import XMonad.Util.Run (spawnPipe, safeSpawn, safeSpawnProg)

-- Layout
import XMonad.Layout (Full)
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.IndependentScreens
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.LimitWindows (setLimit)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Reflect (Reflect, reflectHoriz)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.LayoutCombinators (NewSelect, (|||))
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.LimitWindows (Selection, limitSelect)
import XMonad.Layout.ToggleLayouts (ToggleLayouts, toggleLayouts)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL))
import XMonad.Layout.Fullscreen (FullscreenFull, fullscreenEventHook, fullscreenFull)

-- | VARIABLES

myModMask :: KeyMask
myModMask = mod4Mask      -- Sets modKey to command/super key

myTerminal :: String
myTerminal = "alacritty"  -- Sets default terminal

myBrowser :: String
myBrowser = "google-chrome-stable" -- Sets edge for tree select

myEditor :: String
myEditor = "emacsclient -c -a emacs" -- Sets emacs as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 2           -- Window Border

myNormColor :: String
myNormColor   = "#282c34"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myWorkspaces :: [String]
myWorkspaces = [ " 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 " ]

-- | LAYOUTS

-- Layouts
myLayoutHook =
  avoidStruts 
  . smartBorders 
  . toggleLayouts Full
  . limitSelect 1 5
  . fullScreenToggle 
  $ (tiled ||| Mirror tiled ||| column3 ||| full ||| tall ||| reflectedTall ||| twopane ||| grid)
 where
  -- Gaps bewteen windows
  myGaps gap       = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
  gapSpaced g      = spacing g . myGaps g

  nmaster          = 1 -- Default number of windows
  ratio            = (/) 1 2 -- Default screen proportion
  delta            = (/) 3 100 -- Default screen percent when resizes

  tiled            = gapSpaced 10 $ Tall nmaster delta ratio
  full             = gapSpaced 5 Full
  column3          = gapSpaced 10 $ ThreeColMid 1 (3 / 100) (1 / 2)

  tall             = ResizableTall 1 0.03 (φ / (1 + φ)) []
  reflectedTall    = reflectHoriz tall
  twopane          = TwoPane 0.03 (1 / φ)
  grid             = Grid
  φ                = realToFrac ((1.0 + sqrt 5) / 2.0 :: Double)

  fullScreenToggle = mkToggle (single NBFULL)

-- | HOOKS

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

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> fullscreenEventHook

mdspManageHook :: ManageHook
mdspManageHook = composeAll $ concat
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

-- | KEYBINDINGS

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
           , ("g", spawn "emacsclient -c --eval '(emacs-everywhere)'")
           ]
         )

    --- My Applications
       , ("M-t", spawn (myTerminal ++ " -e ytop"))
       , ("M-w", safeSpawnProg "google-chrome-stable")
       , ("M-m", safeSpawnProg "spotify")
       , ("M-S-m", safeSpawnProg "discord")
       , ("M-c", safeSpawnProg "clipmenu")
       , ("M-s", safeSpawnProg "screenkey")
       , ("M-S-s", safeSpawn "killall" ["screenkey"])
       , ("M-S-p", safeSpawn "flameshot" ["gui", "-p", "/home/matthew/pics/screenshots/"])
       , ("M-C-p", safeSpawn "flameshot" ["screen", "-p", "/home/matthew/pics/screenshots/"])
       
    --- System
       , ("M-x", safeSpawnProg "reboot")
       , ("M-S-x", safeSpawnProg "poweroff")
       , ("M-<Escape>", spawn "betterlockscreen -l dim")
       , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
       , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
       , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
       , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
       , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
       , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ]

-- | MAIN
main :: IO ()
main = do
  n <- countScreens
  -- Launching xmobar.
  xmprocs <- mapM (\i -> spawnPipe $ "xmobar $HOME/.config/xmobar/xmobarrc" ++ " -x " ++ show i) [0..n-1]
  -- the xmonad, ya know...what the WM is named after!
  xmonad $ mkConfig xmprocs

mkConfig xmprocs = ewmh . docks $ myConfig
  where
    keyConfig = myKeys myConfig
    myConfig =
      def
        { modMask            = myModMask,
          terminal           = myTerminal,
          borderWidth        = myBorderWidth,
          normalBorderColor  = myNormColor,
          focusedBorderColor = myFocusColor,
          logHook            = mapM_ (\handle -> myLogHook handle) xmprocs,
          manageHook         = myManageHook <+> mdspManageHook,
          layoutHook         = myLayoutHook,
          handleEventHook    = myHandleEventHook
        }
        `additionalKeysP` keyConfig
