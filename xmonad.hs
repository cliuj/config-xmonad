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
        ("M-C-r", spawn "xmonad --recompile; xmonad --restart"),
        ("M-S-e", io exitSuccess),
        -- Spawn terminal
        ("M-<Return>", spawn myTerminal),
        -- Program launcher
        ("M-<Space>", spawn myProgramLauncher),
        -- Screen locking
        ("M4-l", spawn myLockScreenCmd),
        -- Window management
        ("M-S-w", kill1),
        ("M-S-q", kill1),
        ("M-C-w", killAll),
        -- Workspace management
        ("M-,", sendMessage Shrink),
        ("M-.", sendMessage Expand),
        ("M-S-,", sendMessage (IncMasterN (-1))),
        ("M-S-.", sendMessage (IncMasterN 1)),
        ("M-C-b", sendMessage ToggleStruts),
        ("M-h", prevScreen),
        ("M-l", nextScreen),
        ("M-S-h", sequence_ [shiftPrevScreen, prevScreen]),
        ("M-S-l", sequence_ [shiftNextScreen, nextScreen]),
        -- Navigation
        ("M-j", windows W.focusDown),
        ("M-S-j", windows W.swapDown),
        ("M-k", windows W.focusUp),
        ("M-S-k", windows W.swapUp),
        ("M-m", windows W.focusMaster),
        ("M-S-m", windows W.swapMaster),
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

myLayoutHook :: ModifiedLayout
  AvoidStruts
  (ModifiedLayout Spacing (Choose Tall (Choose (Mirror Tall) Full)))
  Window
myLayoutHook = avoidStruts $ mySpacing 50 $
             layoutHook def
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

