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

import System.IO (hClose, hPutStr, hPutStrLn)
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

main = do
  mySB0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/xmobar0.hs")
  mySB1 <- spawnPipe ("xmobar -x 1 $HOME/.config/xmobar/xmobar1.hs")
  mySB2 <- spawnPipe ("xmobar -x 2 $HOME/.config/xmobar/xmobar2.hs")
  xmonad $ docks $ ewmh $ ewmhFullscreen $ withUrgencyHook NoUrgencyHook def
    { terminal           = "alacritty"
    , modMask            = mod4Mask
    , borderWidth        = 5
    , normalBorderColor  = myBrightBlack
    , focusedBorderColor = myBrightRed
    , workspaces         = myWorkspaces
    , handleEventHook    = myEventHook
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , keys               = myKeys
    , logHook            = dynamicLogWithPP $  filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
        { ppOutput = \x -> hPutStrLn mySB0 x   -- xmobar on monitor 1
                        >> hPutStrLn mySB1 x   -- xmobar on monitor 2
                        >> hPutStrLn mySB2 x   -- xmobar on monitor 3
        }
    } `additionalKeysP` myAdditionalKeys


myPP = xmobarPP
    { ppCurrent = xmobarColor myBrightYellow "" . wrap "<box type=Bottom width=2> " " </box>"
    , ppHidden  = pad
    , ppUrgent  = xmobarColor myBrightYellow myRed . wrap " " " "
    , ppSep     = ""
    , ppWsSep   = ""
    , ppTitle   = xmobarColor myBrightGreen "" . pad
    , ppLayout  = xmobarColor myBrightMagenta ""
    }


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


-- Other configuration options
myWorkspaces :: [ String ]
myWorkspaces = [ "code", "shell", "comms", "web", "media" ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

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

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayoutHook = spacing 2
               $ avoidStruts
               $ mkToggle (NOBORDERS ?? NBFULL ?? EOT) (Mirror tiled ||| tiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 10/100

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

myStartupHook = do
              spawnOnce "nitrogen --restore &"
              spawnOnce "compton &"
              spawn "/usr/local/bin/emacs --daemon" -- emacs daemon for the emacsclient
              spawn "/usr/share/lightdmxrandr.sh"
