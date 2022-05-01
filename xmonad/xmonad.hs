import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Layout.IndependentScreens
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

manageHook' = composeAll
  [ ] --TODO: something

conf' = docks def
  {
    terminal = "urxvtc",
    modMask = mod4Mask, -- optional: use Win key instead of Alt as MODi key
    focusFollowsMouse = False,
    borderWidth = 3,
    normalBorderColor = "#1d2021",
    focusedBorderColor = "#d79921",
    workspaces = withScreens 2 $ map show [1..9],
    manageHook = insertPosition Below Newer <+> manageDocks <+> manageHook',
    layoutHook = avoidStruts . smartBorders
                 $ subTabbed layoutTall
                 ||| Mirror layoutTall
  }
  where
    layoutTall = Tall 1 (3/100) (1/2)

barPP' = xmobarPP
  {
    ppCurrent = xmobarColor "#d79921" "#3c3836" . wrap "[" "]",
    ppHiddenNoWindows = xmobarColor "#a89984" "",
    ppTitle = xmobarColor "#98971a" "" . shorten 32,
    ppSort = getSortByXineramaRule,
    ppOrder = \(ws:l:t:s:_) -> [s,ws,l,t],
    ppExtras = [ screenLog ]
  }

logHook' pp h =
  do
    screenPP 0
    screenPP 1
  where
    marshallCurrentS s = whenCurrentOn s . marshallPP s
    screenPP screen = (dynamicLogWithPP . marshallCurrentS screen) pp
      { ppOutput = hPutStrLn h }

screenLog =
  withWindowSet $ return . Just . ("\xf878 " ++) . getCurrentScreen
  where getCurrentScreen = show . toInteger . W.screen . W.current

main = do
  h <- spawnPipe "xmobar"
  conf' <- return conf' { logHook = logHook' barPP' h }
  xmonad . ewmh $ conf'
    `additionalKeysP` (++)
    [
      ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 2%+"),
      ("<XF86AudioLowerVolume>", spawn "amixer sset Master 2%-"),
      ("<XF86AudioMute>", spawn "amixer sset Master toggle"),
      ("<Print>", spawn "maim | xclip -selection clipboard -t image/png"),
      ("S-<Print>", spawn "maim -s | xclip -selection clipboard -t image/png"),
      ("M-p", spawn "rofi -show run"),
      ("M-z", spawn "emacsclient -c"),
      ("M-S-z", spawn "emacsclient -s xmonad -c ~/dotfiles/xmonad.hs"),
      ("M-b", sendMessage ToggleStruts)
    ]
    [
      (otherModMasks ++ "M-" ++ [key], windows $ onCurrentScreen action tag)
    | (tag, key) <- zip (workspaces' conf') "123456789",
      (otherModMasks, action) <- [ ("", W.greedyView),
                                   ("S-", W.shift) ]
    ]
