import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
-- spawnPipe
import XMonad.Util.Run

main = do
    myStatusBar <- spawnPipe "xmobar"
    xmonad defaultConfig {
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

myLayoutHook = avoidStruts $ layoutHook defaultConfig

myManageHook = manageDocks <+> manageHook defaultConfig

myLogHook h = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn h
}
