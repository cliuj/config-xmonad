-- Base
import XMonad
import qualified XMonad.StackSet as W

-- System
import System.IO (hPutStrLn)
import System.Exit

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Actions.WithAll (killAll)

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, xmobarColor, wrap, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

-- Utils
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)

-- Layout
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier (ModifiedLayout)

-- Data
import Data.Monoid
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

myNormalBorderColor = "#71376A"
myFocusedBorderColor = "#71376A"

myLockScreenCmd :: String
myLockScreenCmd = "betterlockscreen -l"

-----------------------------------------------------------
-- Keybindings
-----------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    [   -- XMonad management
        ("M-C-r", spawn "xmonad --recompile; xmonad --restart"),
        ("M-S-e", io (exitWith ExitSuccess)),
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
    ++ workspaceKeys
    where
        workspaceNumbers = [1 :: Int .. 9] <> [0]
        workspaceKeys =
            [ ("M-" <> m <> show k, withNthWorkspace f i)
            | (k, i) <- zip workspaceNumbers [0 ..]
            , (m, f) <- [("", W.view), ("C-", W.greedyView), ("S-", W.shift)]
            ]

-----------------------------------------------------------
-- Layout 
-----------------------------------------------------------

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

myLayoutHook = avoidStruts $ mySpacing 50 $
             layoutHook def
                where 
                    tall = Tall 1 (3/100) (1/2)

-----------------------------------------------------------
-- LogHook
-----------------------------------------------------------
myLogHook h = dynamicLogWithPP $ xmobarPP
            { ppCurrent = xmobarColor "#c792ea" "" . wrap "[" "]"
            , ppHidden = xmobarColor "#82aaff" "" . wrap "*" ""
            , ppHiddenNoWindows = xmobarColor  "#c792ea" ""
            , ppSep = "<fc=#666666> <fn=1>|</fn> </fc>"
            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
            , ppOrder = \(ws:l:t:ex) -> [ws, l]
            , ppOutput = hPutStrLn h
            }

-----------------------------------------------------------
-- Main
-----------------------------------------------------------
main :: IO()
main = do
    xmonad $ ewmh $ docks def 
        { terminal = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          modMask = myModMask,
          borderWidth = myBorderWidth,
          focusedBorderColor = myFocusedBorderColor,
          normalBorderColor = myNormalBorderColor,
          layoutHook = myLayoutHook
        } `additionalKeysP` myKeys

