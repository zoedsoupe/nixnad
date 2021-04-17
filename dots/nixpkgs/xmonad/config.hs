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
import XMonad
import Data.Ratio
import Data.Monoid (All)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import GHC.IO.Handle.Types (Handle)
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (AvoidStruts, manageDocks, avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat, isDialog)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

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
import qualified XMonad.StackSet as W
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.LimitWindows (setLimit)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Reflect (Reflect, reflectHoriz)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.LayoutCombinators (NewSelect, (|||))
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.LimitWindows (Selection, limitSelect)
import XMonad.Layout.ToggleLayouts (ToggleLayouts, toggleLayouts)
import XMonad.Layout.Fullscreen (FullscreenFull, fullscreenEventHook, fullscreenFull)

-- Polybar
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

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
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]

-- | LAYOUTS

type MyLayout
   = ModifiedLayout AvoidStruts (ModifiedLayout SmartBorder (ModifiedLayout FullscreenFull (ToggleLayouts Full (ModifiedLayout Selection (NewSelect ResizableTall (NewSelect (ModifiedLayout Reflect ResizableTall) (NewSelect TwoPane (NewSelect Grid (NewSelect ThreeCol ThreeCol)))))))))

-- The type of my layouts - not sure there is an easier way to express this
type MyLayouts a
   = NewSelect ResizableTall (NewSelect (ModifiedLayout Reflect ResizableTall) (NewSelect TwoPane (NewSelect Grid (NewSelect ThreeCol ThreeCol)))) a

-- Layouts
layouts :: MyLayouts a
layouts =
  wrkLayout ||| tall ||| reflectedTall ||| twopane ||| grid ||| threecol ||| threecolmid
 where
  -- Gaps bewteen windows
  myGaps gap  = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
  gapSpaced g = spacing g . myGaps g

  tiled         = gapSpaced 10 $ Tall nmaster delta ratio
  full          = gapSpaced 5 Full
  column3       = gapSpaced 10 $ ThreeColMid 1 (3/100) (1/2)

  tall          = ResizableTall 1 0.03 (φ / (1 + φ)) []
  reflectedTall = reflectHoriz tall
  threecol      = ThreeCol 1 (3 / 100) (1 / 2)
  threecolmid   = ThreeColMid 1 (3 / 100) (1 / 2)
  twopane       = TwoPane 0.03 (1 / φ)
  grid          = Grid
  φ             = realToFrac ((1.0 + sqrt 5) / 2.0 :: Double)

  wrkLayout     = onWorkspace wrkWs (tiled ||| full) $ (tiled ||| Mirror tiled ||| column3 ||| full)

-- | HOOKS

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> fullscreenEventHook

-- Layout hook - all quite standard, note the use of limitSelect (I
-- have a key-binding to limit the windows defined below, which is
-- pretty handy)
myLayoutHook :: MyLayout Window
myLayoutHook =
  avoidStruts
    . smartBorders
    . toggleLayouts Full
    . limitSelect 1 5
    $ layouts

msdpManageHook :: ManageHook
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

-- | POLYBAR

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
 in D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue   = "#2E9AFE"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#722222"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          , ppTitle           = wrapper purple . shorten 100
          }

myPolybarLogHook dbus = myLogHook <+> dynamicLogWithPP (polybarHook dbus)

-- | MAIN

main :: IO ()
main = mkDbusClient >>= mkConfig

mkConfig :: D.Client -> IO()
mkConfig dbus = xmonad . ewmh . docks $ myConfig
  where
    keyConfig = myKeys myConfig
    myConfig =
      def
        { modMask            = myModMask,
          terminal           = myTerminal,
          borderWidth        = myBorderWidth,
          normalBorderColor  = myNormColor,
          focusedBorderColor = myFocusColor,
          logHook            = myPolybarLogHook dbus,
          manageHook         = myManageHook <+> mdspManageHook,
          layoutHook         = myLayoutHook,
          handleEventHook    = myHandleEventHook
        }
        `additionalKeysP` keyConfig
