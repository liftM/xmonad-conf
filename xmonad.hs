import           Data.List                      (intercalate)
import           System.Exit                    (exitSuccess)
import           System.IO                      (hPutStrLn)

import           XMonad
import           XMonad.Actions.PhysicalScreens
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks       (ToggleStruts (..), avoidStruts,
                                                 docks, manageDocks)
import           XMonad.Hooks.Place             (placeHook, simpleSmart)
import           XMonad.Hooks.SetWMName         (setWMName)
import           XMonad.Layout.NoBorders        (smartBorders)
import qualified XMonad.StackSet                as SS
import           XMonad.Util.EZConfig           (additionalKeys)
import           XMonad.Util.Loggers            (Logger)
import           XMonad.Util.NamedWindows       (getName, unName)
import           XMonad.Util.Run                (spawnPipe)

-- Keymask names
superMask :: KeyMask
superMask = mod4Mask

defaultMask :: KeyMask
defaultMask = superMask

-- Extra workspaces
customWorkspaces :: [(KeySym, String)]
customWorkspaces =
  zip
    ([xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_BackSpace])
    ((["~"] ++ map show [1 .. 9 :: Integer]) ++ ["0", "-", "=", "B"])

-- Log delimited window titles, highlighting focused
logTitles :: (String -> String) -> Logger
logTitles ppFocus =
  withWindowSet $ fmap (Just . intercalate " | ") . windowTitles
  where
    name = shorten 50 . show
    isFocused windowSet target =
      (== Just (unName target)) $ SS.peek windowSet
    highlightFocused windowSet target =
      (if isFocused windowSet target
         then ppFocus
         else id) $
      name target
    namedWindows = map getName . SS.index
    windowTitles windowSet =
      mapM (fmap $ highlightFocused windowSet) $ namedWindows windowSet

-- Commands
reload :: X ()
reload =
  spawn
    "if type xmonad; then xmonad -- --recompile && xmonad -- --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

main :: IO ()
main
  -- Spawn xmobar
 = do
  spawn "pactl set-card-profile 0 output:hdmi-stereo-extra2" -- Set audio output to HDMI monitor plugged in to speakers
  xmproc <- spawnPipe "xmobar"
  xmonad $
    docks $
    def
    { borderWidth = 2
    , modMask = defaultMask
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , terminal = "terminator"
    , workspaces = map snd customWorkspaces
    , startupHook = setWMName "LG3D" -- Workaround Java Swing applications
    , manageHook = manageDocks <+> placeHook simpleSmart <+> manageHook def
    , layoutHook = avoidStruts $ smartBorders $ layoutHook def
    , logHook =
        dynamicLogWithPP
          xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "green" ""
          , ppExtras = [logTitles (xmobarColor "green" "")]
          , ppOrder = \(ws:l:_:e) -> [ws, l] ++ e
          }
    } `additionalKeys`
      -- Custom kill
    ([ ((defaultMask .|. shiftMask, xK_c), spawn "xkill")
      -- Lock screen
     , ((defaultMask .|. shiftMask, xK_l), spawn "slock")
     , ((defaultMask .|. shiftMask, xK_u), spawn "slock dm-tool lock")
      -- Volume control with media keys
     , ((noModMask, 0x1008ff11), spawn "amixer sset Master 1%- && pactl set-sink-volume alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 -1%")
     , ((noModMask, 0x1008ff12), spawn "amixer sset Master toggle && pactl set-sink-mute alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 toggle")
     , ((noModMask, 0x1008ff13), spawn "amixer sset Master 1%+ && pactl set-sink-volume alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 +1%")
      -- Volume control with function keys
     , ((defaultMask, xK_F10), spawn "amixer sset Master toggle && pactl set-sink-mute alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 toggle")
     , ((defaultMask, xK_F11), spawn "amixer sset Master 1%- && pactl set-sink-volume alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 -1%")
     , ((defaultMask, xK_F12), spawn "amixer sset Master 1%+ && pactl set-sink-volume alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 +1%")
      -- Media player controls with numpad top row
     , ((defaultMask, xK_KP_Divide), spawn "playerctl previous")
     , ((defaultMask, xK_KP_Multiply), spawn "playerctl play-pause")
     , ((defaultMask, xK_KP_Subtract), spawn "playerctl next")
      -- Custom launcher (see `~/.profile` re: $PATH)
     , ((defaultMask, xK_p), spawn "$(yeganesh -x)")
      -- Screenshots
     , ((noModMask, xK_Print), spawn "maim ~/screenshots/$(date +%s).png")
     , ((defaultMask, xK_Print), spawn "maim -s ~/screenshots/$(date +%s).png")
      -- Remap reload => mask-r and physical screens to mask-{q,w,e}
     , ((defaultMask, xK_r), reload)
     , ((defaultMask .|. shiftMask, xK_r), io exitSuccess)
      -- Clipboard manager
     , ((defaultMask, xK_v), spawn "gpaste-client ui")
      -- Toggle struts (xmobar visibility)
     , ((defaultMask, xK_b), sendMessage ToggleStruts)
     ] ++
      -- Workspaces
     [ ((defaultMask .|. extraMask, key), windows $ action workspace)
     | (key, workspace) <- customWorkspaces
     , (action, extraMask) <-
         [(SS.greedyView, noModMask), (SS.shift, shiftMask)]
     ] ++
      -- Physical monitors (triple-headed setup)
     [ ((defaultMask .|. extraMask, key), action def screen)
     | (key, screen) <- zip [xK_q, xK_w, xK_e] [0 ..]
     , (action, extraMask) <-
         [(viewScreen, noModMask), (sendToScreen, shiftMask)]
     ])
