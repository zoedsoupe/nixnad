---  _ __ ___   __| |___ _ __
--- | '_ ` _ \ / _` / __| '_ \
--- | | | | | | (_| \__ \ |_) |
--- |_| |_| |_|\__,_|___/ .__/
---                     |_|

import GHC.IO.Handle.Types (Handle)
import Zoey.Hooks
import Zoey.Keybindings
import Zoey.Layout
import Zoey.Variables
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = do
  -- Launching xmobar.
  handle <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc"
  -- the xmonad, ya know...what the WM is named after!
  xmonad $ mkConfig handle

mkConfig :: Handle -> XConfig MyLayout
mkConfig handle = ewmh . docks $ myConfig
  where
    keyConfig = myKeys myConfig
    myConfig =
      def
        { modMask = myModMask,
          terminal = myTerminal,
          borderWidth = myBorderWidth,
          normalBorderColor = myNormColor,
          focusedBorderColor = myFocusColor,
          logHook = myLogHook handle,
          manageHook = myManageHook <+> pbManageHook,
          layoutHook = myLayoutHook,
          handleEventHook = myHandleEventHook
        }
        `additionalKeysP` keyConfig
