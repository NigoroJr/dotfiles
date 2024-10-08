import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.Master
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

-- Terminal
myTerminal :: String
myTerminal = "alacritty"

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
    onWorkspace "1" (tiled ||| Full) $
    onWorkspaces ["2", "3", "4"] (Full ||| tiled ||| Mirror tiled) $
    onWorkspace "8" (Tall 2 delta ratio) $                      -- 2 x 2
    (Full ||| tiled ||| Mirror tiled)                           -- Otherwise
    where

    tiled = Tall nmaster delta ratio

    nmaster = 1

    ratio = 1/2

    delta = 1/100

-- Manage Hook
myManageHook = manageSpawn <+> myFloatHook <+> manageHook def

-- To get the class name, xprop | grep WM_CLASS
myFloatHook = composeAll
    [ className =? "Chromium-browser"          --> moveTo2
    , className =? "chromium-browser-chromium" --> moveTo2
    , className =? "Google-chrome-stable"      --> moveTo2
    , className =? "Google-chrome"             --> moveTo2
    , className =? "google-chrome"             --> moveTo2
    , className =? "Firefox"                   --> moveTo2
    , className =? "Firefox-esr"               --> moveTo2
    , className =? "jetbrains-clion"           --> moveTo3
    , className =? "Gvim"                      --> moveTo3
    , className =? "Emacs"                     --> moveTo3
    , className =? "Eclipse"                   --> moveTo3
    , className =? "Qpdfview"                  --> moveTo3
    , className =? "qpdfview"                  --> moveTo3
    , className =? "jetbrains-pycharm"         --> moveTo4
    , className =? "mplayer2"                  --> moveTo5
    , className =? "MPlayer"                   --> moveTo5
    , className =? "feh"                       --> moveTo5
    , className =? "Comix"                     --> moveTo5
    , className =? "Geeqie"                    --> moveTo5
    , className =? "Gazeb"                     --> moveTo5
    , className =? "gazebo"                    --> moveTo5
    , className =? "Gimp"                      --> moveTo5
    , className =? "Rviz"                      --> moveTo6
    , className =? "rviz"                      --> moveTo6
    , className =? "Spotify"                   --> moveTo9
    , className =? "spotify"                   --> moveTo9
    , className =? "Java"                      --> doFloat
    , className =? "java-lang-Thread"          --> doFloat
    , className =? "R_x11"                     --> doFloat
    , className =? "Gifview"                   --> doFloat
    , className =? "Gimp"                      --> doFloat
    , role      =? "GtkFileChooserDialog"      --> doCenterFloat
    , manageDocks ]
    where
    -- moveTo1 = doF $ W.shift "1"
    moveTo2 = doF $ W.shift "2"
    moveTo3 = doF $ W.shift "3"
    moveTo4 = doF $ W.shift "4"
    moveTo5 = doF $ W.shift "5"
    moveTo6 = doF $ W.shift "6"
    -- moveTo7 = doF $ W.shift "7"
    -- moveTo8 = doF $ W.shift "8"
    moveTo9 = doF $ W.shift "9"
    role    = stringProperty "WM_WINDOW_ROLE"


-- Log Hook
myLogHook h = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn h,
    ppUrgent = xmobarColor "blue" "white",
    ppTitle  = xmobarColor "green"  "" . shorten 180
}

-- Startup Hook
myStartupHook = do
    setWMName "LG3D"
    spawnOn "1" "alacritty"
    -- spawnOn "2" "firefox"
    -- spawnOn "2" "chromium"
    spawnOn "2" "google-chrome"

-- Mouse bindings
myMouseBindings conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. controlMask, button1),
        (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modMask .|. controlMask, button2),
        (\w -> focus w >> windows W.shiftMaster))
    , ((modMask .|. controlMask, button3),
        (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

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
    -- , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    -- , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    -- , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask .|. shiftMask, xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask .|. shiftMask, xK_l     ), sendMessage Expand)

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
    xmonad $ docks $ withUrgencyHook NoUrgencyHook def {
        terminal = myTerminal
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        , manageHook = myManageHook
        , layoutHook = myLayoutHook
        , handleEventHook = handleEventHook def
        , logHook = myLogHook myStatusBar
        , startupHook = myStartupHook
        , keys = myKeys
        , mouseBindings = myMouseBindings
    }
