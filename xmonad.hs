module Main
  ( main
  )
where

import           System.Exit                              ( exitSuccess )
import           System.IO                                ( IO
                                                          , hPutStrLn
                                                          )

import           Data.Bool                                ( Bool(..) )
import           Data.Int                                 ( Int )
import           Data.String                              ( String )
import           Data.Function                            ( id
                                                          , ($)
                                                          , (.)
                                                          )

import           Data.Tuple                               ( snd )
import           Data.List                                ( (++)
                                                          , zip
                                                          , intercalate
                                                          )
import           Data.Maybe                               ( Maybe(Just) )

import           Data.Eq                                  ( (==) )
import           Data.Functor                             ( Functor(fmap) )
import           Control.Monad                            ( mapM )

import           Text.Show                                ( Show(show) )

import           XMonad
import           XMonad.Actions.PhysicalScreens
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks                 ( ToggleStruts(..)
                                                          , avoidStruts
                                                          , docks
                                                          , manageDocks
                                                          )
import           XMonad.Hooks.Place                       ( placeHook
                                                          , simpleSmart
                                                          )
import           XMonad.Hooks.SetWMName                   ( setWMName )
import           XMonad.Layout.NoBorders                  ( smartBorders )
import qualified XMonad.StackSet               as SS
import           XMonad.Util.EZConfig                     ( additionalKeys )
import           XMonad.Util.Loggers                      ( Logger )
import           XMonad.Util.NamedWindows                 ( getName
                                                          , unName
                                                          )
import           XMonad.Util.Run                          ( spawnPipe )
import           XMonad.Prompt                            ( XPPosition(..)
                                                          , font
                                                          , position
                                                          , XPConfig
                                                          )
import           XMonad.Actions.DynamicWorkspaces         ( withWorkspace
                                                          , removeEmptyWorkspace
                                                          , selectWorkspace
                                                          )


main :: IO ()
main = do
  -- Set audio output to HDMI monitor plugged in to speakers
  spawn "pactl set-card-profile 0 output:hdmi-stereo-extra2"
  -- Spawn xmobar
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ (xConf xmproc) `additionalKeys` keybindings

-- XMonad configuration
xConf outputHandle = def
  { borderWidth       = 2
  , modMask           = defaultMask
  , focusFollowsMouse = False
  , clickJustFocuses  = False
  , terminal          = "terminator"
  , workspaces        = fmap snd customWorkspaces
  -- Workaround for Java Swing applications: https://stackoverflow.com/questions/30742662/java-swing-gui-not-displaying-in-xmonad
  , startupHook       = setWMName "LG3D"
  , manageHook        = manageDocks <+> placeHook simpleSmart <+> manageHook def
  , layoutHook        = avoidStruts $ smartBorders $ layoutHook def
  , logHook           = dynamicLogWithPP xmobarPP
                          { ppOutput = hPutStrLn outputHandle
                          , ppTitle  = xmobarColor "green" ""
                          , ppExtras = [logTitles (xmobarColor "green" "")]
                          , ppOrder  = \(ws : l : _ : e) -> [ws, l] ++ e
                          }
  }

-- Extra workspaces
customWorkspaces :: [(KeySym, String)]
customWorkspaces = zip
  ([xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_BackSpace])
  ((["~"] ++ fmap show [1 .. 9 :: Int]) ++ ["0", "-", "=", "B"])

-- Log truncated pipe-delimited window titles, highlighting focused window
logTitles :: (String -> String) -> Logger
logTitles ppFocus =
  withWindowSet $ fmap (Just . intercalate " | ") . windowTitles
 where
  name = shorten 50 . show
  isFocused windowSet target = (== Just (unName target)) $ SS.peek windowSet
  highlightFocused windowSet target =
    (if isFocused windowSet target then ppFocus else id) $ name target
  namedWindows = fmap getName . SS.index
  windowTitles windowSet =
    mapM (fmap $ highlightFocused windowSet) $ namedWindows windowSet

-- Prompt configuration
xpConf :: XPConfig
xpConf = def { position = Bottom, font = "xft:monospace" }

-- Keymask names
superMask :: KeyMask
superMask = mod4Mask

defaultMask :: KeyMask
defaultMask = superMask

-- Custom keybindings
keybindings :: [((KeyMask, KeySym), X ())]
keybindings =
  [
      -- Custom kill
    ( (defaultMask .|. shiftMask, xK_c)
    , spawn "xkill"
    )
      -- Lock screen
    , ((defaultMask .|. shiftMask, xK_l), spawn "slock")
    , ( (defaultMask .|. shiftMask, xK_u)
      , spawn "slock dm-tool lock"
      )
      -- Volume control with media keys
    , ( (noModMask, 0x1008ff11)
      , spawn
        "amixer sset Master 1%- && pactl set-sink-volume alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 -1%"
      )
    , ( (noModMask, 0x1008ff12)
      , spawn
        "amixer sset Master toggle && pactl set-sink-mute alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 toggle"
      )
    , ( (noModMask, 0x1008ff13)
      , spawn
        "amixer sset Master 1%+ && pactl set-sink-volume alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 +1%"
      )
      -- Volume control with function keys
    , ( (defaultMask, xK_F10)
      , spawn
        "amixer sset Master toggle && pactl set-sink-mute alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 toggle"
      )
    , ( (defaultMask, xK_F11)
      , spawn
        "amixer sset Master 1%- && pactl set-sink-volume alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 -1%"
      )
    , ( (defaultMask, xK_F12)
      , spawn
        "amixer sset Master 1%+ && pactl set-sink-volume alsa_output.pci-0000_01_00.1.hdmi-stereo-extra2 +1%"
      )
      -- Media player controls with numpad top row
    , ((defaultMask, xK_KP_Divide)  , spawn "playerctl previous")
    , ((defaultMask, xK_KP_Multiply), spawn "playerctl play-pause")
    , ( (defaultMask, xK_KP_Subtract)
      , spawn "playerctl next"
      )
      -- Custom launcher (see `~/.profile` re: $PATH)
    , ( (defaultMask, xK_p)
      , spawn "$(yeganesh -x)"
      )
      -- Dynamic workspaces
    , ((defaultMask, xK_backslash), selectWorkspace xpConf)
    , ( (defaultMask .|. shiftMask, xK_backslash)
      , withWorkspace xpConf (windows . SS.shift)
      )
    , ( (defaultMask, xK_Delete)
      , removeEmptyWorkspace
      )
      -- Screenshots
    , ((noModMask, xK_Print), spawn "maim ~/screenshots/$(date +%s).png")
    , ( (defaultMask, xK_Print)
      , spawn "maim -s ~/screenshots/$(date +%s).png"
      )
      -- Remap reload => mask-r and physical screens to mask-{q,w,e}
    , ( (defaultMask, xK_r)
      , do
        ok <- recompile False
        if ok then restart "xmonad" True else spawn "xmessage recompile failed"
      )
    , ( (defaultMask .|. shiftMask, xK_r)
      , io exitSuccess
      )
      -- Clipboard manager
    , ( (defaultMask, xK_v)
      , spawn "gpaste-client ui"
      )
      -- Toggle struts (xmobar visibility)
    , ((defaultMask, xK_b), sendMessage ToggleStruts)
    ]
    ++
      -- Workspaces
       [ ((defaultMask .|. extraMask, key), windows $ action workspace)
       | (key, workspace) <- customWorkspaces
       , (action, extraMask) <-
         [(SS.greedyView, noModMask), (SS.shift, shiftMask)]
       ]
    ++
      -- Physical monitors (triple-headed setup)
       [ ((defaultMask .|. extraMask, key), action def screen)
       | (key, screen) <- zip [xK_q, xK_w, xK_e] [0 ..]
       , (action, extraMask) <-
         [(viewScreen, noModMask), (sendToScreen, shiftMask)]
       ]
