-- Base
import XMonad
import qualified XMonad.StackSet as W

-- System
import System.IO (hPutStrLn)
import System.Exit ( exitSuccess )

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
    ( nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, nextWS, prevWS )
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Actions.WithAll (killAll)

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, xmobarColor, wrap, PP(..))
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageDocks
    ( avoidStruts, AvoidStruts, docks, ToggleStruts(ToggleStruts))

-- Utils
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)

-- Layout
import XMonad.Layout.Spacing
    ( spacingRaw
    , Border(Border)
    , Spacing )
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.IndependentScreens (onCurrentScreen, withScreens, workspaces', countScreens)
import XMonad.Layout.BinarySpacePartition (emptyBSP, Rotate(Rotate), ResizeDirectional(..))
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Util.Types (Direction2D(L, R, U, D))
import XMonad.Layout.WindowNavigation (windowNavigation, Navigate(..))
import XMonad.Layout.Renamed (renamed, Rename(Replace))

-- Data
import Data.Monoid ()
import Data.Maybe(fromMaybe)
import qualified Data.Map as M

-----------------------------------------------------------
-- Variables
-----------------------------------------------------------
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myTerminal :: String
myTerminal = "alacritty"

myEditor :: String
myEditor = myTerminal ++ " -e nvim"

myProgramLauncher :: String
myProgramLauncher = "rofi -show run"

myBrowser :: String
myBrowser = "firefox"

-- Alt as the mod key
myModMask :: KeyMask
myModMask = mod1Mask

myBorderWidth :: Dimension
myBorderWidth = 0

myNormalBorderColor :: String
myNormalBorderColor = "#71376A"

myFocusedBorderColor :: String
myFocusedBorderColor = "#71376A"

myLockScreenCmd :: String
myLockScreenCmd = "betterlockscreen -l"

myWorkspaces :: [String]
myWorkspaces = [ "term", "code", "browser", "comm" ]

-----------------------------------------------------------
-- Layout-specific actions
-----------------------------------------------------------

-- Helper to get current layout description
getLayoutDescription :: X String
getLayoutDescription = do
    ws <- gets windowset
    return $ description . W.layout . W.workspace . W.current $ ws

-- Execute action based on current layout
layoutAction :: [(String, X ())] -> String -> X ()
layoutAction actions key = do
    layout <- getLayoutDescription
    let action = case layout of
            "BSP" -> lookup key bspActions
            "Tall" -> lookup key tallActions
            "Mirror Tall" -> lookup key mirrorTallActions
            "Monocle" -> lookup key monocleActions
            "Fullscreen" -> lookup key fullscreenActions
            _ -> Nothing
    fromMaybe (return ()) action

-----------------------------------------------------------
-- Keybindings
-----------------------------------------------------------
--
-- BSP layout actions
bspActions :: [(String, X ())]
bspActions =
    [ ("h", sendMessage $ Go L)
    , ("j", sendMessage $ Go D)
    , ("k", sendMessage $ Go U)
    , ("l", sendMessage $ Go R)
    , ("S-h", sendMessage $ Swap L)
    , ("S-j", sendMessage $ Swap D)
    , ("S-k", sendMessage $ Swap U)
    , ("S-l", sendMessage $ Swap R)
    , ("C-h", sendMessage $ ShrinkFrom R)
    , ("C-j", sendMessage $ ExpandTowards D)
    , ("C-k", sendMessage $ ShrinkFrom D)
    , ("C-l", sendMessage $ ExpandTowards R)
    , ("s", sendMessage BSP.Swap)
    , ("r", sendMessage Rotate)
    ]

-- Tall layout actions
tallActions :: [(String, X ())]
tallActions =
    [ ("j", windows W.focusDown)
    , ("k", windows W.focusUp)
    , ("S-j", windows W.swapDown)
    , ("S-k", windows W.swapUp)
    , ("h", sendMessage Shrink)
    , ("l", sendMessage Expand)
    ]

-- Mirror Tall layout actions
mirrorTallActions :: [(String, X ())]
mirrorTallActions =
    [ ("h", windows W.focusUp)
    , ("l", windows W.focusDown)
    , ("S-h", windows W.swapUp)
    , ("S-l", windows W.swapDown)
    , ("j", sendMessage Shrink)
    , ("k", sendMessage Expand)
    ]

-- Monocle layout actions (Full with gaps)
monocleActions :: [(String, X ())]
monocleActions =
    [ ("j", windows W.focusDown)
    , ("k", windows W.focusUp)
    ]

-- Fullscreen layout actions (true fullscreen)
fullscreenActions :: [(String, X ())]
fullscreenActions =
    [ ("j", windows W.focusDown)
    , ("k", windows W.focusUp)
    ]

-- Layout independent keybindings
myKeys :: [(String, X ())]
myKeys =
    [   -- XMonad management
        ("M-C-r", spawn "xmonad --recompile && xmonad --restart && notify-send 'Xmonad' 'Recompiled and restarted successfully'"),
        ("M-S-e", io exitSuccess),
        -- Spawn terminal
        ("M-<Return>", spawn myTerminal),
        -- Program launcher
        ("M-<Space>", spawn myProgramLauncher),
        -- Screen locking
        ("M4-l", spawn myLockScreenCmd),
        -- Window management
        ("M-w", kill1),
        ("M-S-q", kill1),
        ("M-C-w", killAll),
        -- Layout management
        ("M-[", sendMessage NextLayout),
        ("M-]", sendMessage NextLayout),  -- Both cycle forward for now
        -- Workspace navigation
        ("M-<Tab>", nextWS),
        ("M-S-<Tab>", prevWS),
        ("M-C-,", sendMessage Shrink),
        ("M-C-.", sendMessage Expand),
        ("M-C-S-,", sendMessage (IncMasterN (-1))),
        ("M-C-S-.", sendMessage (IncMasterN 1)),
        ("M-C-b", sendMessage ToggleStruts),
        -- Screen navigation (DWM-like)
        ("M-,", prevScreen),
        ("M-.", nextScreen),
        ("M-S-,", sequence_ [shiftPrevScreen, prevScreen]),
        ("M-S-.", sequence_ [shiftNextScreen, nextScreen]),
        -- Layout-aware navigation
        ("M-h", layoutAction [] "h"),
        ("M-j", layoutAction [] "j"),
        ("M-k", layoutAction [] "k"),
        ("M-l", layoutAction [] "l"),
        ("M-S-h", layoutAction [] "S-h"),
        ("M-S-j", layoutAction [] "S-j"),
        ("M-S-k", layoutAction [] "S-k"),
        ("M-S-l", layoutAction [] "S-l"),
        ("M-C-l", layoutAction [] "C-l"),
        ("M-C-h", layoutAction [] "C-h"),
        ("M-C-j", layoutAction [] "C-j"),
        ("M-C-k", layoutAction [] "C-k"),
        ("M-s", layoutAction [] "s"),
        ("M-r", layoutAction [] "r"),
        -- Applications
        ("M-b", spawn myBrowser)
    ] ++
    -- Workspace switching (with IndependentScreens)
    [ ("M-" ++ show k, windows $ onCurrentScreen W.greedyView i)
        | (i, k) <- zip myWorkspaces [1..] ] ++
    -- Move window to workspace
    [ ("M-S-" ++ show k, windows $ onCurrentScreen W.shift i)
        | (i, k) <- zip myWorkspaces [1..] ]

-----------------------------------------------------------
-- Layout 
-----------------------------------------------------------

-- Helper for spacing
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

-- Individual layout definitions with custom modifiers
-- BSP with 40px spacing
bspLayout = renamed [Replace "BSP"] 
          $ avoidStruts 
          $ windowNavigation 
          $ mySpacing 40 
          $ emptyBSP

-- Tall with 30px spacing
tallLayout = renamed [Replace "Tall"] 
           $ avoidStruts 
           $ windowNavigation 
           $ mySpacing 30 
           $ Tall 1 (3/100) (1/2)

-- Mirror Tall with 30px spacing
mirrorTallLayout = renamed [Replace "Mirror Tall"] 
                 $ avoidStruts 
                 $ windowNavigation 
                 $ mySpacing 30 
                 $ Mirror (Tall 1 (3/100) (1/2))

-- Full (monocle) with spacing
monocleLayout = renamed [Replace "Monocle"] 
              $ avoidStruts 
              $ windowNavigation 
              $ mySpacing 40 
              $ Full

-- True fullscreen without any gaps or struts
fullscreenLayout = renamed [Replace "Fullscreen"] 
                 $ windowNavigation 
                 $ Full

-- Combined layout hook
myLayoutHook = bspLayout 
           ||| tallLayout 
           ||| mirrorTallLayout 
           ||| monocleLayout 
           ||| fullscreenLayout

-----------------------------------------------------------
-- Main
-----------------------------------------------------------
main :: IO()
main = do
    screensCount <- countScreens
    xmonad $ ewmh $ docks def 
        { terminal = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          modMask = myModMask,
          workspaces = withScreens screensCount myWorkspaces,
          borderWidth = myBorderWidth,
          focusedBorderColor = myFocusedBorderColor,
          normalBorderColor = myNormalBorderColor,
          layoutHook = myLayoutHook
        } `additionalKeysP` myKeys

