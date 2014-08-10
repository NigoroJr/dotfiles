import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Util.Run
-- import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Master

main = do
    myStatusBar <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
        terminal = "urxvt"
        , borderWidth = 1
        , normalBorderColor = "black"
        , focusedBorderColor = "blue"

        --, modMask = myModMask
        , layoutHook = myLayoutHook
        , manageHook = myManageHook
        , logHook = myLogHook myStatusBar
        , startupHook = setWMName "LG3D"
    }

-- myModMask = mod4Mask

-- myLayoutHook = avoidStruts $ layoutHook defaultConfig
myLayoutHook = avoidStruts $ smartBorders $ ((mastered (3/100) (28/100) $ Tall nmaster delta (46/50)) ||| Full ||| tiled ||| Mirror tiled)
    where
    -- three_col = ThreeCol nmaster delta ratio

    tiled = Tall nmaster delta ratio

    nmaster = 1

    ratio = 1/2

    delta = 1/100

myManageHook = manageDocks <+> manageHook defaultConfig

myLogHook h = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn h
}
