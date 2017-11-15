-------------------------------------------------------------------------------
-- Import modules                                                           {{{
-------------------------------------------------------------------------------
import qualified Data.Map as M
import Control.Monad (liftM2)          -- myManageHookShift
import Data.Monoid
import System.IO                       -- for xmobar
import XMonad
import qualified XMonad.StackSet as W  -- myManageHookShift
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo(runOrRaise)
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog         -- for xmobar
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Util.EZConfig(additionalKeysP, removeKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import Graphics.X11.ExtraTypes.XF86 -- keycord
import XMonad.Util.WorkspaceCompare (getSortByIndex) --scratchpad dependence

-- 1.work 2.web 3.other
myWorkspaces = ["1", "2", "3"]
modm = mod4Mask
colorBlue = "#90caf9"
colorGreen = "#9ccc65"
colorRed = "#ea80fc"
colorGray = "#546E7A"
colorWhite = "#eceff1"
colorGrayAlt = "#b0bec5"
colorNormalbg = "#263238"
colorfg = "#CFD8DC"
mynormalBorderColor  = colorGrayAlt
myfocusedBorderColor = "#e53935"

main :: IO ()

main = do
    wsbar <- spawnPipe myWsBar
    xmonad $ docks $ ewmh defaultConfig
       { borderWidth        = 1
       , terminal           = "termite"
       , focusFollowsMouse  = True
       --, normalBorderColor  = mynormalBorderColor
       --, focusedBorderColor = myfocusedBorderColor
       , startupHook        = myStartupHook
       , manageHook         = insertPosition End Newer <+>
                              namedScratchpadManageHook myScratchpads <+>
                              manageDocks <+>
                              myManageHookShift
        , layoutHook         = avoidStruts(noBorders (tabbed shrinkText myTabConfig))
       , logHook            = myLogHook wsbar
                                >> updatePointer (0.5, 0.5) (0, 0) 
       , workspaces         = myWorkspaces
       , modMask            = modm
       }

       -------------------------------------------------------------------- }}}
       -- Define keys to remove                                             {{{
       ------------------------------------------------------------------------

       `removeKeysP`
       [
       -- Unused gmrun binding
       "M-S-p"
       ]

       -------------------------------------------------------------------- }}}
       -- Keymap: window operations                                         {{{
       ------------------------------------------------------------------------

       `additionalKeysP`
       [
         ("<F11>",   namedScratchpadAction myScratchpads "qtermite1")
       , ("<F12>",   namedScratchpadAction myScratchpads "qtermite2")
       , ("M3-v",     namedScratchpadAction myScratchpads "vim-help")
       ]
       `additionalKeysP`
       [
       -- Lock screen
         ("M1-C-l", spawn "light-locker-command -l")
       -- Toggle compton (compsite manager)
       , ("M1-C-t", spawn "bash $HOME/script/toggle-compton.sh")
       -- Launch terminal
       , ("M5-<Return>", spawn "termite")
       -- file manager -> dired
       -- Launch web browser
       , ("M5-l",   spawn "bash script/Line.sh")
       -- Launch emacs
       , ("M5-e",   spawn "bash script/emacsServer.sh")
       , ("M5-S-e",   spawn "bash script/emacsTerminal.sh")
       , ("M5-C-e",   spawn "bash script/emacsStop.sh")
       , ("M5-v",   spawn "virt-manager")
       , ("M5-n",   spawn "termite -e neovim")
       -- Launch web browser secret-mode
       , ("M-a",   runOrRaise "google-chrome-stable" (resource =? "google-chrome"))
       , ("M-C-a", runOrRaise "google-chrome-stable --incognito --new-window" (resource =? "google-chrome"))
       -- Launch dmenu for launching applicatiton
       , ("M-p", runOrRaisePrompt myXPConfig)
       -- Play / Pause media keys
       , ("<XF86AudioPlay>"  , spawn "mpc toggle")
       -- Raise Volume setting media keys
       , ("<XF86AudioRaiseVolume>", spawn "sound_volume_change_wrapper.sh +")
       , ("S-<XF86AudioRaiseVolume>", spawn "sound_volume_change_wrapper2.sh +")
       , ("C-<XF86AudioRaiseVolume>", spawn "sound_volume_change_wrapper3.sh +")
       -- Lower Volume setting media keys
       , ("<XF86AudioLowerVolume>", spawn "sound_volume_change_wrapper.sh -")
       , ("S-<XF86AudioLowerVolume>", spawn "sound_volume_change_wrapper2.sh -")
       , ("C-<XF86AudioLowerVolume>", spawn "sound_volume_change_wrapper3.sh -")
        -- Brightness Keys
       , ("<XF86MonBrightnessUp>"  , spawn "xbacklight + 5 -time 100 -steps 1")
       , ("S-<XF86MonBrightnessUp>"  , spawn "xbacklight + 1 -time 100 -steps 1")
       , ("C-<XF86MonBrightnessUp>"  , spawn "xbacklight + 10 -time 100 -steps 1")
       , ("<XF86MonBrightnessDown>", spawn "xbacklight - 5 -time 100 -steps 1")
       , ("S-<XF86MonBrightnessUp>"  , spawn "xbacklight - 1 -time 100 -steps 1")
       , ("C-<XF86MonBrightnessUp>"  , spawn "xbacklight - 10 -time 100 -steps 1")
       -- Take a screenshot (whole window)
       , ("<Print>", spawn "screenshot.sh")
       -- Take a screenshot (selected area)
       , ("S-<Print>", spawn "screenshot_select.sh")
       -- Launch ipython qtconsole
       -- Toggle touchpad
       , ("C-<Escape>", spawn "bash $HOME/script/touchpad_toggle.sh")
       ]

--------------------------------------------------------------------------- }}}
-- myStartupHook:     Start up applications                                 {{{
-------------------------------------------------------------------------------

myStartupHook = do
        spawn "bash $HOME/script/monitor.sh"
        spawn "bash $HOME/.fehbg"
       -- spawn "bash $HOME/script/keymap.sh"

--------------------------------------------------------------------------- }}}
-- myManageHookShift: some window must created there                        {{{
-------------------------------------------------------------------------------
-- ウィンドウ作成時のデフォルトワークスペースを指定
myManageHookShift = composeAll
            -- if you want to know className, type "$ xprop|grep CLASS" on shell
            [ 
              className =? "Emacs"       --> mydoShift "1"
            , resource =? "google-chrome"       --> mydoShift "2"
            , className =? "Virt-manager"       --> mydoShift "3"
            , className =? "Virt-viewer"       --> mydoShift "3"
            , resource =? "crx_menkifleemblimdogmoihpfopnplikde"       --> mydoShift "2"
            ]
             where mydoShift = doF . liftM2 (.) W.greedyView W.shift

--------------------------------------------------------------------------- }}}
-- myLogHook:         loghock settings                                      {{{
-------------------------------------------------------------------------------

myLogHook h = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ wsPP { ppOutput = hPutStrLn h }
--------------------------------------------------------------------------- }}}
-- myWsBar:           xmobar setting                                        {{{
-------------------------------------------------------------------------------

myWsBar = "xmobar $HOME/.xmobar/desktop.hs"

wsPP = xmobarPP { ppOrder           = \(ws:l:t:_)  -> [ws,t]
                , ppCurrent         = xmobarColor colorRed   colorNormalbg . const "●"
                , ppUrgent          = xmobarColor colorGray    colorNormalbg . const "●"
                , ppVisible         = xmobarColor colorRed    colorNormalbg . const "⦿"
                , ppHidden          = xmobarColor colorGray    colorNormalbg . const "●"
                , ppHiddenNoWindows = xmobarColor colorGray    colorNormalbg . const "○"
                , ppTitle           = \str -> ""
                , ppSort            = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
                , ppOutput          = putStrLn
                , ppWsSep           = ""
                , ppSep = "|"
                }
-- XPConfig                                           
myXPConfig = defaultXPConfig
                { font              = "xft:Ricty:size=18:antialias=true"
                , fgColor           = colorfg
                , bgColor           = colorNormalbg
                , borderColor       = colorNormalbg
                , height            = 26
                , promptBorderWidth = 0
                , promptKeymap      = emacsLikeXPKeymap
                , changeModeKey     = xK_grave
                , autoComplete      = Just 100000
                , bgHLight          = colorfg
                , fgHLight          = colorNormalbg
                , position          = Top
                , historyFilter     = deleteAllDuplicates 
                , completionKey     = (controlMask , xK_i)
                }
-- scratchpad
myScratchpads = [
                 NS "qtermite1"  "termite -t qtermite1" (title =? "qtermite1")
                     (customFloating $ W.RationalRect 0 0.03 1 0.47)
                , NS "qtermite2"  "termite -t qtermite2" (title =? "qtermite2")
                      (customFloating $ W.RationalRect 0 0.5 1 0.5)
                , NS "vim-help" "termite -t vim-help -e 'nvim -c help'" (title =? "vim-help")
                   (customFloating $ W.RationalRect 0 0.03 1 0.97)
                ]
notSP = return $ ("NSP" /=) . W.tag
nextWS' = moveTo Next (WSIs notSP)
prevWS' = moveTo Prev (WSIs notSP)
-- tab
myTabConfig =  def { 
                       activeColor         = colorRed
                     , inactiveColor       = colorfg
                     , urgentColor         = colorBlue
                     , activeBorderColor   = colorRed
                     , inactiveBorderColor = colorGray
                     , urgentBorderColor   = colorBlue
                     , activeTextColor     = colorNormalbg
                     , inactiveTextColor   = colorNormalbg
                     , urgentTextColor     = colorNormalbg
                     , fontName            = "xft:Ricty:size=14:bold:antialis=true"
                     , decoWidth           = 200
                     , decoHeight          = 20
                     , windowTitleAddons   = []
                     , windowTitleIcons    = []
                 }
