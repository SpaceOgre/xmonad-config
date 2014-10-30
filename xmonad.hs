-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Actions.Navigation2D
import XMonad.Actions.Search as S
import XMonad.Actions.Submap as SM
import XMonad.Prompt as P
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Combo
import XMonad.Layout.Named
import XMonad.Layout.MultiColumns
import XMonad.Layout.Grid
--import XMonad.Layout.HintedGrid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.LayoutHints
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Text.Regex.Posix ((=~))


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "xterm"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:Term","2:Code","3:Web"] ++ map show [4..9]


------------------------------------------------------------------------
-- Window rules
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

q ~? x = fmap (=~ x) q

myManageHook = composeAll
    [
      -- Tecsas
      title ~? "VxSim" --> doShift "6:VxSim",
     -- title =? "selnvmtc318:eqm02s05 (VxSim:SS7)" --> doShift "6:VxSim",
     -- title =? "selnvmtc318:eqm02s09 (VxSim:Routing)" --> doShift "6:VxSim",
     -- title =? "selnvmtc318:eqm02s02 (VxSim:Routing)" --> doShift "6:VxSim",
     -- title =? "selnvmtc318:eqm02s03 (VxSim:Payload)" --> doShift "6:VxSim",
     -- title =? "selnvmtc318:eqm02s06 (VxSim:Payload)" --> doShift "6:VxSim",
     -- title =? "selnvmtc318:eqm02s07 (VxSim:Payload)" --> doShift "6:VxSim",
     -- stringProperty "WM_CLIENT_MACHINE" =? "selnvmtc318" --> doShift "3:Tecsas",
      title ~? "Linux" --> doShift "3:Tecsas",
      title ~? "Erlang" --> doShift "3:Tecsas",
      title ~? "GTT GUI" --> doShift "3:Tecsas"
    ]


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ onWorkspace "3:Tecsas" myGrid $ standardLayouts
  where
    standardLayouts = tall ||| myTabbed ||| myGrid ||| myMultiCol
    myGrid     = named "Grid" $ noFrillsDeco shrinkText tabConfig $ Grid
    myMultiCol = multiCol [1] 3 0.01 (-0.5)
    myTabbed   = named "Tabbed" $ tabbed shrinkText tabConfig
    tall       = Tall 1 (3/100) (1/2)
    --tecsas     = named "Tecsas" (combineTwo (TwoPane (3/100) (3/4)) (myGrid) (myTabbed))


------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 2


-- Search engine bindings
searchEngineMap method = M.fromList $
        [ ((0, xK_g), method S.google)
        , ((0, xK_i), method S.images)
        , ((0, xK_s), method S.scholar)
        , ((0, xK_d), method S.dictionary)
        , ((0, xK_t), method S.thesaurus)
        , ((0, xK_m), method S.maps)
        , ((0, xK_w), method S.wikipedia)
        , ((0, xK_y), method S.youtube)
        ]


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

    -- Launch dmenu via yeganesh.
    -- Use this to launch programs without a key binding.
  , ((modMask, xK_p), 
     spawn "dmenu-with-yeganesh")
    
    --
  , ((modMask, xK_s), SM.submap $ searchEngineMap $ S.promptSearch defaultXPConfig)

    -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

    
    --------------------------------------------------------------------
    -- Layout key bindings
    --
    
    -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

    
    --------------------------------------------------------------------
    -- Movement key bindings
    --

    -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

    -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

    -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

    -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

    -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

    -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

    -- Shrink the master area.
  , ((modMask, xK_y),
     sendMessage Shrink)

    -- Expand the master area.
  , ((modMask, xK_u),
     sendMessage Expand) 

    -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

    -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))
    
    -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

    -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  ++

  -- Window navigation for Wmii-Like layout
  [ ((modMask, xK_Right), windowGo R False)
  , ((modMask, xK_Left ), windowGo L False)
  , ((modMask, xK_Up   ), windowGo U False)
  , ((modMask, xK_Down ), windowGo D False)
  , ((modMask .|. shiftMask, xK_Right), windowSwap R False)
  , ((modMask .|. shiftMask, xK_Left ), windowSwap L False)
  , ((modMask .|. shiftMask, xK_Up   ), windowSwap U False)
  , ((modMask .|. shiftMask, xK_Down ), windowSwap D False)]
  

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
      }
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
  }


------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
}
