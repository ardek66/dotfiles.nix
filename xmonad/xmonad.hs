import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Layout.IndependentScreens
import XMonad.Layout.SubLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.BoringWindows

import XMonad.Layout.Simplest
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W hiding (focusUp, focusDown)
import XMonad.Actions.WindowGo
import XMonad.Layout.WindowNavigation
import XMonad.Actions.Navigation2D
import System.Environment
import XMonad.Util.Replace
import XMonad.Core
import System.IO
import Data.Semigroup
import XMonad.Layout.LayoutModifier
import XMonad.StackSet hiding (focusUp, focusDown)
import XMonad.Actions.CycleWS
import XMonad.Actions.TagWindows
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Grid

manageHook' :: Query (Endo WindowSet)
manageHook' =
    composeOne [ className =? "yabridge-host.exe.so" -?> hasBorder False <+> doIgnore <+> doRaise
               , className =? "yabridge-host-32.exe.so" -?> hasBorder False <+> doIgnore <+> doRaise
               , transience
               , isFullscreen -?> doFullFloat
               , isDialog -?> doCenterFloat
               , className =? "QjackCtl" -?> doFloat
               , className =? "REAPER" -?> hasBorder False
	       , className =? "plasmashell" -?> doFloat
               ]
barPP' :: PP
barPP' =
  xmobarPP
  {
    ppCurrent = xmobarColor "#d79921" "#3c3836" . wrap "[" "]",
    ppHiddenNoWindows = xmobarColor "#a89984" "",
    ppTitle = xmobarColor "#98971a" "" . shorten 32,
    ppSort = getSortByXineramaRule,
    ppOrder = \(ws:l:t:s:_) -> [s,ws,l,t],
    ppExtras = [ screenLog ]
  }

logHook' :: PP -> Handle -> X ()
logHook' pp h =
  do
    screenPP 0
    screenPP 1
  where
    marshallCurrentS s = whenCurrentOn s . marshallPP s
    screenPP screen = (dynamicLogWithPP . marshallCurrentS screen) pp
      { ppOutput = hPutStrLn h }

screenLog :: X (Maybe [Char])
screenLog =
  withWindowSet $ return . Just . ("\xf878 " ++) . getCurrentScreen
  where getCurrentScreen = show . toInteger . W.screen . W.current

main :: IO ()
main =
  do
    replace
    xmobar <- getEnv "XMONAD_XMOBAR"
    h <- spawnPipe xmobar
    let
      layoutTall = subLayout [] Simplest $ Tall 1 (3/100) (1/2)
      conf' = docks def
              { terminal = "urxvtc"
              , modMask = mod4Mask -- optional: use Win key instead of Alt as MODi key
              , focusFollowsMouse = False
              , borderWidth = 3
              , normalBorderColor = "#1d2021"
              , focusedBorderColor = "#d79921"
              , XMonad.Core.workspaces = withScreens 2 $ map show [1..9]
              , manageHook = insertPosition End Newer <+> manageHook kdeConfig <+> manageHook'
              , layoutHook = boringWindows . avoidStruts . smartBorders
                             $ layoutTall
                             ||| Mirror layoutTall
                             ||| Grid
              , handleEventHook = swallowEventHook (className =? "URxvt") (return True)
              , logHook = logHook' barPP' h
          }
    xmonad . ewmhFullscreen . ewmh $ conf'
      `additionalKeysP` (++)
      [ ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 2%+")
      , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 2%-")
      , ("<XF86AudioMute>", spawn "amixer sset Master toggle")
      , ("<Print>", spawn "maim | xclip -selection clipboard -t image/png")
      , ("S-<Print>", spawn "maim -s | xclip -selection clipboard -t image/png")
      , ("M-q", spawn "xmonad --restart")
      , ("M-p", spawn "rofi -show run")
      , ("M-c", spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'")
      , ("M-z", spawn "emacsclient -c")
      , ("M-b", sendMessage ToggleStruts)
      , ("M-C-n", withFocused (sendMessage . UnMerge))
      , ("M-C-S-j", withFocused (sendMessage . mergeDir W.focusDown'))
      , ("M-C-S-k", withFocused (sendMessage . mergeDir W.focusUp'))
      , ("M-C-j", onGroup W.focusDown')
      , ("M-C-k", onGroup W.focusUp')
      , ("M-j", focusDown)
      , ("M-k", focusUp)
      , ("M-=", nextWS)
      , ("M--", prevWS)
      , ("M-S-=", shiftToNext >> nextWS)
      , ("M-S--", shiftToPrev >> prevWS)
      , ("M-f", moveTo Next emptyWS)
      , ("M-S-f", do t <- findWorkspace getSortByIndex Next emptyWS 1
                     (windows . shift) t
                     (windows . greedyView) t)
      , ("M-\\", toggleWS)
      ]
      [ (otherModMasks ++ "M-" ++ [key], windows $ onCurrentScreen action tag)
      | (tag, key) <- zip (workspaces' conf') "123456789",
        (otherModMasks, action) <- [ ("", W.greedyView),
                                     ("S-", W.shift) ]
      ]
