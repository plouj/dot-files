import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import Data.Ratio
import XMonad.Layout.LayoutHints
import XMonad.Hooks.FadeInactive
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map        as M
import XMonad.Hooks.Place
import XMonad.Util.EZConfig(additionalKeysP, additionalKeys, removeKeysP)
import System
import System.Exit(exitWith)
import XMonad.Actions.PhysicalScreens -- http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Actions-PhysicalScreens.html


--Fading
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
   where fadeAmount = 0.9

-- Workspaces

myWorkspaces = map show [1 .. 9 :: Int]

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modMask, button5), (\_ -> moveTo Next NonEmptyWS))
    , ((modMask, button4), (\_ -> moveTo Prev NonEmptyWS ))

    , ((modMask .|. shiftMask, button5), (\w -> focus w >> kill ))
    ]

-- Layout options:
myLayout = avoidStruts $ smartBorders $ layoutHints (resizableTile ||| Mirror resizableTile ||| Grid ||| Full)
     where
        resizableTile = ResizableTall nmaster delta ratio []
        nmaster = 1
        ratio = toRational (2/(1+sqrt(5)::Double))
        delta = 1/100

myManageHook = placeHook simpleSmart

main = xmonad $ ewmh $ gnomeConfig
       { workspaces = myWorkspaces
       , modMask = mod4Mask -- use the Windows button as mod
       , terminal = "gnome-terminal"
       , normalBorderColor  = "#333333"
       , focusedBorderColor = "#0088FF"
       , layoutHook = myLayout
       , manageHook = manageHook gnomeConfig <+> myManageHook
       , mouseBindings      = myMouseBindings
       , logHook = myLogHook
       } `additionalKeysP`  -- M is the meta key, S is the shift key
       [ ("M-S-t",       kill)
       , ("M-q",       withFocused $ windows . W.sink)
       , ("M-c",       restart "xmonad" True)
       , ("M-S-c",     spawn "gnome-session-save --kill")
       , ("M-S-l",     spawn "gnome-screensaver-command -l")
       , ("M-S-s",     spawn "scrot")
       , ("M-`",       gnomeRun) -- This is just a workaround for a
                                 -- gnome-settings-daemon bug
                                 -- (https://bugs.launchpad.net/ubuntu/+source/gnome-settings-daemon/+bug/694910
                                 -- and
                                 -- https://bugzilla.gnome.org/show_bug.cgi?id=651571)
       ] `additionalKeys`
       [ ((mod4Mask .|. mask, key), f sc) -- have to use mod4Mask explicitly instead of the previously defined modMask
             | (key, sc) <- zip [xK_w, xK_e] [0..]
             , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)] ]
       `removeKeysP` ["M-p"]
    where
      myManageHook = composeAll . concat $
                     [ [ className   =? c --> doF (W.shift "1") | c <- webApps]
                     , [composeOne [ isFullscreen -?> doFullFloat ]]
                     ]
      webApps       = ["Firefox", "chrome", "Chrome"] -- open on desktop 1
