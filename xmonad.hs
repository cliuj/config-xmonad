-- Base
import XMonad
import qualified XMonad.StackSet as W

-- System
import System.IO (hPutStrLn)
import System.Exit ( exitSuccess )

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
    ( nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen )
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

-- Data
import Data.Monoid ()
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
-- Keybindings
-----------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    [   -- XMonad management
        ("M-C-r", spawn "xmonad --recompile && xmonad --restart && notify-send 'Xmonad' 'Recompiled and restarted successfully'"),
        ("M-S-e", io exitSuccess),
        -- Spawn Editor
        ("M-<Return> e", spawn myEditor),
        -- Spawn terminal
        ("M-<Return> t", spawn myTerminal),
        -- Program launcher
        ("M-<Space>", spawn myProgramLauncher),
        -- Screen locking
        ("M4-l", spawn myLockScreenCmd),
        -- Window management
        ("M-S-w", kill1),
        ("M-S-q", kill1),
        ("M-C-w", killAll),
        -- Workspace management
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
        -- Directional window focus (BSPWM-like, confined to current screen)
        ("M-h", sendMessage $ Go L),
        ("M-j", sendMessage $ Go D),
        ("M-k", sendMessage $ Go U),
        ("M-l", sendMessage $ Go R),
        -- Directional window swapping
        ("M-S-h", sendMessage $ Swap L),
        ("M-S-j", sendMessage $ Swap D),
        ("M-S-k", sendMessage $ Swap U),
        ("M-S-l", sendMessage $ Swap R),
        -- Traditional stack-based navigation (as fallback)
        ("M-<Tab>", windows W.focusDown),
        ("M-S-<Tab>", windows W.focusUp),
        ("M-m", windows W.focusMaster),
        ("M-S-m", windows W.swapMaster),
        -- BinarySpacePartition controls
        ("M-C-l", sendMessage $ ExpandTowards R),
        ("M-C-h", sendMessage $ ShrinkFrom R),
        ("M-C-j", sendMessage $ ExpandTowards D),
        ("M-C-k", sendMessage $ ShrinkFrom D),
        ("M-s", sendMessage BSP.Swap),
        ("M-r", sendMessage Rotate),
        -- Applications
        ("M-f", spawn myBrowser)
    ]
myAdditionalKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myAdditionalKeys conf = let modm = modMask conf in M.fromList $
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-----------------------------------------------------------
-- Layout 
-----------------------------------------------------------

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

myLayoutHook = avoidStruts $ windowNavigation $ mySpacing 40 $
             emptyBSP ||| tall ||| Mirror tall ||| Full
                where 
                    tall = Tall 1 (3/100) (1/2)

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
          keys = myAdditionalKeys,
          layoutHook = myLayoutHook
        } `additionalKeysP` myKeys

