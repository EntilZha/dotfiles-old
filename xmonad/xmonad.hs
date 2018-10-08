import Data.Map as Map
import XMonad
import XMonad.Util.WorkspaceCompare
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig


solarized :: Map String String
solarized = Map.fromList [
    ("base03",   "#002b36"),
    ("base02",   "#073642"),
    ("base01",   "#586e75"),
    ("base00",   "#657b83"),
    ("base0",    "#839496"),
    ("base1",    "#93a1a1"),
    ("base2",    "#eee8d5"),
    ("base3",    "#fdf6e3"),
    ("yellow",   "#b58900"),
    ("orange",   "#cb4b16"),
    ("red",      "#dc322f"),
    ("magenta",  "#d33682"),
    ("violet",   "#6c71c4"),
    ("blue",     "#268bd2"),
    ("cyan",     "#2aa198"),
    ("green", "#859900")]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { terminal = "gnome-terminal"
    , modMask = mod4Mask
    , manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts $ layoutHook def
    , handleEventHook = handleEventHook def <+> docksEventHook
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor (Map.findWithDefault "" "orange" solarized) "" . wrap "[" "]"
      , ppUrgent = xmobarColor (Map.findWithDefault "" "yellow" solarized) "" . wrap "*" "*" . xmobarStrip
      , ppSep = "  ——  "
      , ppTitle = xmobarColor (Map.findWithDefault "" "blue" solarized) ""
      }
    , startupHook = setWMName "LG3D"
    , focusedBorderColor = "#236ea3"
    , workspaces = ["1:term", "2:code", "3:web", "4:msg", "5:music", "6", "7", "8", "9"]
    } `additionalKeysP` myKeys


myKeys =
    [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]

