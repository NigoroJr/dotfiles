import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Layout.Master
import XMonad.Layout.PerWorkspace

import qualified XMonad.StackSet as W

-- Terminal
myTerminal :: String
myTerminal = "urxvt"

-- Border
myBorderWidth :: Dimension
myBorderWidth = 1
myNormalBorderColor = "black"
myFocusedBorderColor = "blue"

-- Workspaces
myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

-- Layout Hook
myLayoutHook = avoidStruts $ smartBorders $
    onWorkspaces ["2", "3"] (Full ||| tiled ||| Mirror tiled) $
    onWorkspace "4" (Tall 2 delta ratio)                      $ -- 2x2
    (mastered_tall ||| Full ||| tiled ||| Mirror tiled)         -- Otherwise
    where
    mastered_tall = mastered (3/100) (28/100) $ Tall nmaster delta (46/50)

    tiled = Tall nmaster delta ratio

    nmaster = 1

    ratio = 1/2

    delta = 1/100

-- Manage Hook
myManageHook = manageSpawn <+> myFloatHook <+> manageHook defaultConfig

myFloatHook = composeAll
    [ className =? "Chromium-browser"   --> moveTo2
    , className =? "Gvim"               --> moveTo3
    , className =? "Emacs"              --> moveTo3
    , className =? "Eclipse"            --> moveTo3
    , className =? "mplayer2"           --> moveTo5
    , className =? "Java"               --> doFloat
    , manageDocks ]
    where
    moveTo2 = doF $ W.shift "2"
    moveTo3 = doF $ W.shift "3"
    moveTo5 = doF $ W.shift "5"

-- Log Hook
myLogHook h = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn h
}

-- Startup Hook
myStartupHook = do
    --setWMName "SG3D"
    spawnOn "1" "urxvt"
    spawnOn "1" "urxvt"
    spawnOn "1" "urxvt"
    spawnOn "2" "optirun chromium"
    spawn "imwheel -b \"0 0 6 7 0 0 10\" -k"
    spawnOn "3" "gvim"
    spawnOn "4" "urxvt"
    spawnOn "4" "urxvt"
    spawnOn "4" "urxvt"
    spawnOn "4" "urxvt"

main = do
    myStatusBar <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
        terminal = myTerminal
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        , layoutHook = myLayoutHook
        , manageHook = myManageHook
        , logHook = myLogHook myStatusBar
        , startupHook = myStartupHook
    }
