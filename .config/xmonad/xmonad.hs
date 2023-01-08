--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--


-- https://github.com/brianbuccola/dotfiles/blob/main/xmonad/.xmonad/xmonad.hs

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.NamedScratchpad


-- Lookup
import XMonad.Hooks.EwmhDesktops      -- use EWMH hints
import XMonad.Hooks.UrgencyHook       -- colorize urgent WSs
import XMonad.Actions.DynamicProjects -- make WSs projects, dynamically create, rename, move projects

import Graphics.X11.ExtraTypes.XF86   -- bind media keys
-- Added by me
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Layout.IndependentScreens
    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S



import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import Graphics.X11.Xinerama (getScreenInfo)

import XMonad.Actions.OnScreen (onlyOnScreen)
import XMonad.Actions.Warp (warpToScreen)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Colors.GruvboxDark

main = xmonad
     . ewmhFullscreen
     . ewmh
     . dynamicSBs myStatusBarSpawner
     . docks
     $ myConfig

myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
                    pure $ statusBarPropTo ("_XMONAD_LOG_" ++ show s)
                          ("xmobar -x " ++ show s ++ " ~/.config/xmobar/xmobar" ++ show s ++ ".hs")
                          (pure $ myPP (S s))

myPP :: ScreenId -> PP
myPP s = filterOutWsPP [scratchpadWorkspaceTag] . marshallPP s $ def
  { ppCurrent = xmobarColor myBrightYellow "" . wrap "<box type=Bottom width=2> " " </box>"
    , ppHidden  = pad
    , ppHiddenNoWindows  = pad
    , ppUrgent  = xmobarColor myBrightYellow myRed . wrap " " " "
    , ppSep     = ""
    , ppWsSep   = ""
    , ppTitle   = xmobarColor myBrightGreen "" . pad

    }


myConfig = withUrgencyHook NoUrgencyHook def
    { terminal           = "alacritty"
    , modMask            = mod4Mask
    , borderWidth        = 5
    , normalBorderColor  = myBrightBlack
    , focusedBorderColor = myBrightRed
    , workspaces         = withScreens 2 [ "code", "shell", "comms", "web", "media" ]
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , keys               = myKeys
    , logHook            = myLogHook
    , handleEventHook    = myEventHook
    } `additionalKeysP` myAdditionalKeys
    -- `additionalMouseBindings` myButtons

myFont = "xft:Dina:size=12"

-- The preferred default programs, which is used in a binding below and by
--
myBrowser :: String
myBrowser      = "qutebrowser "  -- Sets qutebrowser as browser

myMusicplayer :: String
myMusicplayer  = "spotify"

myEditor :: String
myEditor       = "emacsclient -c"

myTerminal :: String
myTerminal     = "alacritty"

myComms :: String
myComms        = "discord"

myApplauncher :: String
myApplauncher  = "dmenu_run"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 3

myModMask       = mod4Mask

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"


myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    [
      ("M-S-q",                kill)
    , ("M-C-<Space>",          sendMessage (MT.Toggle NBFULL))
    --, ("M-S-<Space>",        setLayout $ XMonad.layoutHook conf)
    , ("M-C-n",                refresh)
    , ("M-C-<Tab>",            sendMessage NextLayout)

-- Window control
    , ("M-C-<Down>",           windows W.focusDown)
    , ("M-C-<Up>",             windows W.focusUp)
    , ("M-C-S-<Down>",         windows W.swapDown)
    , ("M-C-S-<Up>",           windows W.swapUp)
    , ("M-C-<Left>",           windows W.focusMaster)
    --, ("M-C-<Right>",          windows W.focus)
    , ("M-C-<Backspace>",      windows W.swapMaster)
    , ("M-C-m",                sendMessage Expand)
    , ("M-C-k",                sendMessage Shrink)
    , ("M-C-t",                withFocused $ windows . W.sink)
    , ("M-comma",              sendMessage (IncMasterN 1))
    , ("M-period",             sendMessage (IncMasterN (-1)))
    , ("M-S-x",                io (exitWith ExitSuccess))
    , ("M-q",                  spawn "killall xmobar; xmonad --recompile; xmonad --restart")

-- Applications
    , ("M-j",                  spawn myEditor)
    , ("M-l",                  spawn myTerminal)
    , ("M-u",                  spawn myComms)
    , ("M-y",                  spawn myBrowser)
    , ("M-'",                  spawn myMusicplayer)
    , ("M-C-<Return>",         spawn myTerminal)
    , ("M-S-C-<Return>",       spawn myApplauncher)
    ]
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    --
    -- mod-[1..9], Switch to workpace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
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

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayoutHook = spacing 2 $ avoidStruts $ mkToggle (NOBORDERS ?? NBFULL ?? EOT) (Mirror tiled ||| tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
              spawnOnce "nitrogen --restore &"
              spawnOnce "compton &"
              spawn "/usr/local/bin/emacs --daemon" -- emacs daemon for the emacsclient
              spawn "/usr/share/lightdmxrandr.sh"
------------------------------------------------------------------------

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
