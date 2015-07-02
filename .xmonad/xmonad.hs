import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Master
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

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
    onWorkspace "1" (mastered_tall ||| Full ||| tiled ||| Mirror tiled) $
    onWorkspaces ["2", "3"] (Full ||| tiled ||| Mirror tiled) $
    onWorkspace "4" (Tall 2 delta ratio)                      $ -- 2x2
    (Full ||| tiled ||| Mirror tiled)                           -- Otherwise
    where
    mastered_tall = mastered (3/100) (28/100) $ Tall nmaster delta (46/50)

    tiled = Tall nmaster delta ratio

    nmaster = 1

    ratio = 1/2

    delta = 1/100

-- Manage Hook
myManageHook = manageSpawn <+> myFloatHook <+> manageHook defaultConfig

-- To get the class name, xprop | grep WM_CLASS
myFloatHook = composeAll
    [ className =? "Chromium-browser"   --> moveTo2
    , className =? "Gvim"               --> moveTo3
    , className =? "Emacs"              --> moveTo3
    , className =? "Eclipse"            --> moveTo3
    , className =? "Qpdfview"           --> moveTo3
    , className =? "mplayer2"           --> moveTo5
    , className =? "MPlayer"            --> moveTo5
    , className =? "feh"                --> moveTo5
    , className =? "Comix"              --> moveTo5
    , className =? "Geeqie"             --> moveTo5
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
    spawnOn "2" "chromium"
    spawn "imwheel -b \"0 0 6 7 0 0 10\" -k"
    spawnOn "3" "gvim"
    spawnOn "4" "urxvt"
    spawnOn "4" "urxvt"
    spawnOn "4" "urxvt"
    spawnOn "4" "urxvt"

-- Keybindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu_run
    --, ((modMask,               xK_p     ), spawn "dmenu_run")
    , ((modMask,               xK_q     ), spawn "dmenu_run")

    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    --, ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    --, ((modMask,               xK_q     ), restart "xmonad" True)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [ ((m .|. modMask, k), windows $ f i)
         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_apostrophe, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    -- ++
    -- Spawn Vim
    -- [ ((modMask,                 xK_r           ), spawn "gvim")
    -- ]

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
        , keys = myKeys
    }
