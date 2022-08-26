import XMonad
    ( mod4Mask,
      spawn,
      withWindowSet,
      (|||),
      xmonad,
      (<+>),
      composeAll,
      sendMessage,
      windows,
      withFocused,
      Default(def),
      XConfig(terminal, modMask, focusFollowsMouse, borderWidth,
              normalBorderColor, focusedBorderColor, workspaces, manageHook,
              layoutHook, logHook, handleEventHook),
      Mirror(Mirror),
      Tall(Tall),
      className,
      (=?), (-->), doFloat )

import XMonad.Hooks.ManageDocks
    ( avoidStruts, docks, manageDocks, ToggleStruts(ToggleStruts) )

import XMonad.Hooks.ManageHelpers ( (-?>), composeOne, isFullscreen, doFullFloat, transience )
import XMonad.Hooks.WindowSwallowing ( swallowEventHook )
import XMonad.Hooks.DynamicLog
    ( def,
      dynamicLogWithPP,
      shorten,
      wrap,
      xmobarColor,
      xmobarPP,
      PP(ppCurrent, ppHiddenNoWindows, ppTitle, ppSort, ppOrder,
         ppExtras, ppOutput) )

import XMonad.Hooks.InsertPosition
    ( insertPosition, Focus(Newer), Position(Below) )

import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhFullscreen )
import XMonad.Util.Run ( hPutStrLn, spawnPipe )
import XMonad.Util.EZConfig ( additionalKeysP )
import XMonad.Layout.IndependentScreens
    ( marshallPP,
      onCurrentScreen,
      whenCurrentOn,
      withScreens,
      workspaces' )

import XMonad.Layout.SubLayouts
    ( mergeDir, onGroup, subLayout, GroupMsg(UnMerge), pushWindow )

import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.BoringWindows
    ( boringWindows, focusDown, focusUp )

import XMonad.Layout.Simplest ( Simplest(Simplest) )
import XMonad.Util.WorkspaceCompare ( getSortByXineramaRule )
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowGo (doF)
import XMonad.Layout.WindowNavigation (Navigate(Go))
import XMonad.Actions.Navigation2D (Direction2D(U))
import System.Environment (getEnv)
import XMonad.Util.Replace (replace)
import XMonad.Core ( X, WindowSet, Query)
import System.IO (Handle)
import Data.Semigroup (Endo)
import XMonad.Layout.LayoutModifier (LayoutModifier)

manageHook' :: Query (Endo WindowSet)
manageHook' =
    composeOne [ transience
               , className =? "QjackCtl" -?> doFloat
               , isFullscreen -?> doFullFloat
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
              , workspaces = withScreens 2 $ map show [1..9]
              , manageHook = insertPosition Below Newer <+> manageDocks <+> manageHook'
              , layoutHook = boringWindows . avoidStruts . smartBorders
                             $ layoutTall
                             ||| Mirror layoutTall
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

      ]
      [ (otherModMasks ++ "M-" ++ [key], windows $ onCurrentScreen action tag)
      | (tag, key) <- zip (workspaces' conf') "123456789",
        (otherModMasks, action) <- [ ("", W.greedyView),
                                     ("S-", W.shift) ]
      ]
