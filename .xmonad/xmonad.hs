import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
-- spawnPipe
import XMonad.Util.Run

main = do
    myStatusBar <- spawnPipe "xmobar"
    xmonad defaultConfig {
        terminal = "rxvt"
        , borderWidth = 1
        , normalBorderColor = "black"
        , focusedBorderColor = "blue"

        --, modMask = myModMask
        , layoutHook = myLayoutHook
        , manageHook = myManageHook
        , logHook = myLogHook myStatusBar
    }

-- myModMask = mod4Mask

myLayoutHook = avoidStruts $ layoutHook defaultConfig

myManageHook = manageDocks <+> manageHook defaultConfig

myLogHook h = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn h
}
