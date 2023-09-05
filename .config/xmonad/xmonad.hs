-- Dependencies:
-- Alacritty
-- Rofi
-- Fonts defined in xmobar

import XMonad
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86   -- bind media keys
import Graphics.X11.Xinerama (getScreenInfo)
import System.Exit
import System.IO (hClose, hPutStr, hPutStrLn)

    -- Actions
import XMonad.Actions.Navigation2D
import XMonad.Actions.OnScreen (onlyOnScreen)
import XMonad.Actions.DynamicProjects -- make WSs projects, dynamically create, rename, move projects
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

       -- Layouts modifiers
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Colors.GruvboxDark

main = do
  mySB0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/xmobar0.hs")
  mySB1 <- spawnPipe ("xmobar -x 1 $HOME/.config/xmobar/xmobar0.hs")
  mySB2 <- spawnPipe ("xmobar -x 2 $HOME/.config/xmobar/xmobar0.hs")
  xmonad $ navigation2DP def
                        ("<Up>", "<Left>", "<Down>", "<Right>")
                       [("M-C-",   windowGo  ),
                        ("M-C-S-", windowSwap)]
                       False
    $ docks
    $ ewmh
    $ ewmhFullscreen
    $ withUrgencyHook NoUrgencyHook def
    { terminal           = "alacritty"
    , modMask            = mod4Mask
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    , handleEventHook    = myEventHook
    , startupHook        = myStartupHook
    , manageHook         = manageSpawn <> myManageHook
    , layoutHook         = myLayoutHook
    , keys               = myKeys
    , logHook            = dynamicLogWithPP $  filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
        { ppOutput = \x -> hPutStrLn mySB0 x   -- xmobar on monitor 1
                        >> hPutStrLn mySB1 x   -- xmobar on monitor 2
                        >> hPutStrLn mySB2 x   -- xmobar on monitor 3
        , ppCurrent = xmobarColor "#ff79c6" "" . currentWorkspace
        , ppVisible = xmobarColor "#d4bfff" "" . occupiedWorkspace
        , ppHidden = xmobarColor "#d4bfff" "" . occupiedWorkspace
        , ppHiddenNoWindows = xmobarColor "#d4bfff" "" . occupiedWorkspace
        , ppSep =  "<fc=#212733>    </fc>"
        , ppWsSep =  "<fc=#212733> </fc>"
        , ppTitle = xmobarColor myWhite "" . pad
        }
    } `additionalKeysP` myAdditionalKeys

myFont :: String
myFont = "xft:Dina:size=12"

-- Preferred programs
myBrowser :: String
myBrowser = "firefox"

myMusicplayer :: String
myMusicplayer = "spotify"

myEditor :: String
myEditor = "emacsclient -c"

myTerminal :: String
myTerminal = "alacritty"

myComms :: String
myComms = "discord"

myScreenshot :: String
myScreenshot = "flameshot gui"

myApplauncher :: String
myApplauncher = "rofi -show run -theme gruvbox-dark"

-- Workspaces
currentWorkspace :: String -> String
currentWorkspace  _ = "<fn=2>\61713</fn>"

occupiedWorkspace :: String -> String
occupiedWorkspace  _ = "<fn=3>\61713</fn>"

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
    where
        doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = (map xmobarEscape) $ ["1", "2", "3", "4", "5" ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Mouse behavior
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Border properties
myBorderWidth   = 3
myNormalBorderColor  = myBlue
myFocusedBorderColor = myBrightYellow

-- Default modmask -- mod4Mask = WinKey
myModMask       = mod4Mask

-- Keymap
myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    [
      ("M-S-q",                kill)
    , ("M-C-<Space>",          sendMessage (MT.Toggle NBFULL))
    --, ("M-S-<Space>",        setLayout $ XMonad.layoutHook conf)
    , ("M-C-n",                refresh)
    , ("M-C-<Tab>",            sendMessage NextLayout)

-- Window navigation keymap for Navigation2D configured in main
-- Window control
--    , ("M-C-<Left>",           sendMessage $ Go L)
--    , ("M-C-<Right>",          sendMessage $ Go R)
--    , ("M-C-<Up>",             sendMessage $ Go U)
--    , ("M-C-<Down>",           sendMessage $ Go D)
--    , ("M-C-S-<Left>",         sendMessage $ Swap L)
--    , ("M-C-S-<Right>",        sendMessage $ Swap R)
--    , ("M-C-S-<Up>",           sendMessage $ Swap U)
--    , ("M-C-S-<Down>",         sendMessage $ Swap D)

    , ("M-C-<Backspace>",      windows W.swapMaster)
    , ("M-C-m",                sendMessage Expand)
    , ("M-C-k",                sendMessage Shrink)
    , ("M-C-t",                withFocused $ windows . W.sink)
    , ("M-comma",              sendMessage (IncMasterN 1))
    , ("M-period",             sendMessage (IncMasterN (-1)))
    , ("M-S-x",                io (exitWith ExitSuccess))
    , ("M-q",                  spawn "killall xmobar; xmonad --recompile; xmonad --restart")

-- Applications
    , ("M-e",                  spawn myEditor)
    , ("M-s",                  spawn myScreenshot)
    , ("M-d",                  spawn myComms)
    , ("M-b",                  spawn myBrowser)
    , ("M-m",                  spawn myMusicplayer)
    , ("M-C-<Return>",         spawn myTerminal)
    , ("M-S-C-<Return>",       spawn myApplauncher)
    ]
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-[1..9], Switch to workpace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
                                                --
    -- mod-S-C-{1, 2, 3},   Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-S-C-A-{1, 2, 3}, Move client to screen 1, 2, or 3
    --
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
        , (f, m) <- [(W.view, shiftMask .|. controlMask), (W.shift, shiftMask .|. controlMask .|. mod1Mask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

------------------------------------------------------------------------
-- Space between Tiling Windows
------------------------------------------------------------------------
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayoutHook = avoidStruts
               $ mkToggle (NOBORDERS ?? NBFULL ?? EOT) (grid ||| tiled ||| Mirror tiled)
  where
     -- default tiling algorithm partitions the screen into two panes
    tiled   = renamed [Replace "Tall"]
     $ mySpacing 2
     $ Tall nmaster delta ratio
    grid = renamed [Replace " Grid"]
     $ mySpacing 2
     $ Grid
    threecolumns = renamed [Replace " ThreeCols"]
     $ mySpacing 2
     $ ThreeCol nmaster delta ratio
    nmaster = 1
    ratio   = 1/3
    delta   = 20/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Spotify"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

myStartupHook = do
              spawnOnce "nitrogen --restore &"
              spawnOnce "compton &"
              spawnOnce "/usr/local/bin/emacs --daemon" -- emacs daemon for the emacsclient
              spawnOnce "/usr/share/lightdmxrandr.sh"
