import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid ()
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
  ( Direction1D (Next, Prev),
    WSType (WSIs),
    moveTo,
    nextScreen,
    nextWS,
    prevScreen,
    prevWS,
    shiftNextScreen,
    shiftPrevScreen,
  )
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Actions.WithAll (killAll)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
  ( AvoidStruts,
    ToggleStruts (ToggleStruts),
    avoidStruts,
    docks,
  )
import XMonad.Hooks.StatusBar (StatusBarConfig, statusBarPropTo, withSB)
import XMonad.Hooks.StatusBar.PP (PP (..), def)
import XMonad.Layout.BinarySpacePartition (ResizeDirectional (..), Rotate (Rotate), emptyBSP)
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.IndependentScreens (countScreens, marshall, marshallPP, marshallSort, onCurrentScreen, withScreens, workspaces')
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.Spacing
  ( Border (Border),
    Spacing,
    spacingRaw,
  )
import XMonad.Layout.WindowNavigation (Navigate (..), windowNavigation)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Types (Direction2D (D, L, R, U))

-----------------------------------------------------------
-- Variables
-----------------------------------------------------------
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myTerminal :: String
myTerminal = "kitty"

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
myWorkspaces = ["term", "code", "browser", "comm"]

-- Interleave workspaces so each screen's Nth workspace is adjacent,
-- ensuring screen K starts on "K_term" rather than "0_<something>".
withScreensInterleaved :: ScreenId -> [String] -> [String]
withScreensInterleaved n wss = [marshall s w | w <- wss, s <- [0 .. n - 1]]

-- Screen-aware workspace cycling
-- Only cycle through workspaces belonging to the current screen
nextWSOnScreen :: X ()
nextWSOnScreen = moveTo Next (WSIs onCurrentScreenPredicate)

prevWSOnScreen :: X ()
prevWSOnScreen = moveTo Prev (WSIs onCurrentScreenPredicate)

-- Predicate that returns True for workspaces on the current screen
onCurrentScreenPredicate :: X (WindowSpace -> Bool)
onCurrentScreenPredicate = do
  ws <- gets windowset
  let screen = W.screen $ W.current ws
      screenWsTags = [marshall screen w | w <- myWorkspaces]
  return (\w -> W.tag w `elem` screenWsTags)

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
  [ ("h", sendMessage $ Go L),
    ("j", sendMessage $ Go D),
    ("k", sendMessage $ Go U),
    ("l", sendMessage $ Go R),
    ("S-h", sendMessage $ Swap L),
    ("S-j", sendMessage $ Swap D),
    ("S-k", sendMessage $ Swap U),
    ("S-l", sendMessage $ Swap R),
    ("C-h", sendMessage $ ShrinkFrom R),
    ("C-j", sendMessage $ ExpandTowards D),
    ("C-k", sendMessage $ ShrinkFrom D),
    ("C-l", sendMessage $ ExpandTowards R),
    ("s", sendMessage BSP.Swap),
    ("r", sendMessage Rotate)
  ]

-- Tall layout actions
tallActions :: [(String, X ())]
tallActions =
  [ ("j", windows W.focusDown),
    ("k", windows W.focusUp),
    ("S-j", windows W.swapDown),
    ("S-k", windows W.swapUp),
    ("h", sendMessage Shrink),
    ("l", sendMessage Expand)
  ]

-- Mirror Tall layout actions
mirrorTallActions :: [(String, X ())]
mirrorTallActions =
  [ ("h", windows W.focusUp),
    ("l", windows W.focusDown),
    ("S-h", windows W.swapUp),
    ("S-l", windows W.swapDown),
    ("j", sendMessage Shrink),
    ("k", sendMessage Expand)
  ]

-- Monocle layout actions (Full with gaps)
monocleActions :: [(String, X ())]
monocleActions =
  [ ("j", windows W.focusDown),
    ("k", windows W.focusUp)
  ]

-- Fullscreen layout actions (true fullscreen)
fullscreenActions :: [(String, X ())]
fullscreenActions =
  [ ("j", windows W.focusDown),
    ("k", windows W.focusUp)
  ]

-- Layout independent keybindings
myKeys :: [(String, X ())]
myKeys =
  [ -- XMonad management
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
    ("M-]", sendMessage NextLayout), -- Both cycle forward for now
    -- Workspace navigation (screen-aware)
    ("M-<Tab>", nextWSOnScreen),
    ("M-S-<Tab>", prevWSOnScreen),
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
  ]
    ++
    -- Workspace switching (with IndependentScreens)
    [ ("M-" ++ show k, windows $ onCurrentScreen W.view i)
      | (i, k) <- zip myWorkspaces [1 ..]
    ]
    ++
    -- Move window to workspace
    [ ("M-S-" ++ show k, windows $ onCurrentScreen W.shift i)
      | (i, k) <- zip myWorkspaces [1 ..]
    ]

-----------------------------------------------------------
-- Layout
-----------------------------------------------------------

-- Helper for spacing
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

-- Individual layout definitions with custom modifiers
-- BSP with 40px spacing
bspLayout =
  renamed [Replace "BSP"] $
    avoidStruts $
      windowNavigation $
        mySpacing 40 $
          emptyBSP

-- Tall with 30px spacing
tallLayout =
  renamed [Replace "Tall"] $
    avoidStruts $
      windowNavigation $
        mySpacing 30 $
          Tall 1 (3 / 100) (1 / 2)

-- Mirror Tall with 30px spacing
mirrorTallLayout =
  renamed [Replace "Mirror Tall"] $
    avoidStruts $
      windowNavigation $
        mySpacing 30 $
          Mirror (Tall 1 (3 / 100) (1 / 2))

-- Full (monocle) with spacing
monocleLayout =
  renamed [Replace "Monocle"] $
    avoidStruts $
      windowNavigation $
        mySpacing 40 $
          Full

-- True fullscreen without any gaps or struts
fullscreenLayout =
  renamed [Replace "Fullscreen"] $
    windowNavigation $
      Full

-- Combined layout hook
myLayoutHook =
  bspLayout
    ||| tallLayout
    ||| mirrorTallLayout
    ||| monocleLayout
    ||| fullscreenLayout

-----------------------------------------------------------
-- StatusBar
-----------------------------------------------------------

-- Get the layout description for a specific screen (not just the focused one)
screenLayout :: ScreenId -> X (Maybe String)
screenLayout sid = do
  ws <- gets windowset
  let allScreens = W.current ws : W.visible ws
      target = [s | s <- allScreens, W.screen s == sid]
  return $ case target of
    (s : _) -> Just $ description $ W.layout $ W.workspace s
    [] -> Nothing

-- Build a PP that outputs: "state:name:index|..." for each workspace, then "|||layout"
mkPP :: ScreenId -> ScreenId -> [String] -> PP
mkPP screenId nScreens baseWs =
  let allWs = withScreensInterleaved nScreens baseWs
      wsIdx ws = fromMaybe 0 $ elemIndex (marshall screenId ws) allWs
      fmt state ws = state ++ ":" ++ ws ++ ":" ++ show (wsIdx ws)
      basePP =
        def
          { ppCurrent = fmt "current",
            ppVisible = fmt "visible",
            ppHidden = fmt "hidden",
            ppHiddenNoWindows = fmt "empty",
            ppUrgent = fmt "urgent",
            ppWsSep = "|",
            ppSep = "|||",
            ppLayout = const "",
            ppTitle = const "",
            ppExtras = [screenLayout screenId],
            ppOrder = \xs -> case xs of
              (ws : _ : _ : layout : _) -> [ws, layout]
              (ws : _ : _) -> [ws, "BSP"]
              _ -> xs
          }
   in (marshallPP screenId basePP)
        { ppSort = fmap (marshallSort screenId) (ppSort basePP)
        }

myStatusBar :: ScreenId -> ScreenId -> [String] -> StatusBarConfig
myStatusBar screenId nScreens baseWs =
  statusBarPropTo propName spawnCmd (pure $ mkPP screenId nScreens baseWs)
  where
    propName = "_XMONAD_LOG_" ++ show (fromEnum screenId)
    barName = "top-bar-" ++ show (fromEnum screenId)
    spawnCmd = "eww open " ++ barName

-----------------------------------------------------------
-- Main
-----------------------------------------------------------
main :: IO ()
main = do
  screensCount <- countScreens
  let allWs = withScreensInterleaved screensCount myWorkspaces
      sbs =
        mconcat
          [ myStatusBar (S s) screensCount myWorkspaces
            | s <- [0 .. fromEnum screensCount - 1]
          ]
  xmonad $
    ewmh $
      docks $
        withSB sbs $
          def
            { terminal = myTerminal,
              focusFollowsMouse = myFocusFollowsMouse,
              modMask = myModMask,
              workspaces = allWs,
              borderWidth = myBorderWidth,
              focusedBorderColor = myFocusedBorderColor,
              normalBorderColor = myNormalBorderColor,
              layoutHook = myLayoutHook
            }
            `additionalKeysP` myKeys
